<template>
  <div class="game-wrapper">
    <!-- Colonne de gauche : ChatBot -->
    <div class="chat-column">
      <h2>PBot – Bot explicateur</h2>
      <p>
        Bonjour, je suis PBot, le bot explicateur du jeu PontuXL.<br />
        En quoi puis-je vous aider ?
      </p>

      <div class="chat-history">
        <div
          v-for="(msg, idx) in chatHistory"
          :key="idx"
          :class="msg.from"
        >
          <strong>{{ msg.from }} :</strong>
          <div class="chat-message">{{ msg.text }}</div>
        </div>
      </div>

      <div class="chat-input">
        <input
          v-model="chatQuestion"
          @keyup.enter="sendChat"
          placeholder="Posez votre question…"
        />
        <button @click="sendChat">Envoyer</button>
      </div>
    </div>

    <!-- Colonne de droite : Plateau de jeu -->
    <div class="board-column">
      <div class="board-container">
        <h2>PontuXL – 6×6</h2>

        <div
          v-if="feedbackMessage"
          class="feedback-banner"
          :class="{ error: feedbackIsError }"
        >
          {{ feedbackMessage }}
        </div>

        <CircleGrid
          :rows="6"
          :cols="6"
          :lutins="lutins"
          :bridges="bridges"
          :highlightIntersections="highlightIntersections"
          @lutin-clicked="onLutinClicked"
          @intersection-clicked="onIntersectionClicked"
          @pont-action="handlePontAction"
        />

        <div v-if="pendingMove && !pendingActionType" class="warning-banner">
          ⚠ Choisissez d’abord :
          <button @click="startBridgeAction('remove')">Supprimer un pont</button>
          <button @click="startBridgeAction('rotate')">Tourner un pont</button>
        </div>

        <h3>Tour actuel : {{ currentPlayer }}</h3>
        <h3>Ponts restants : {{ bridges.length }}</h3>

        <div>
          <h4>Ponts supprimés : {{ deletedBridges.length }}</h4>
          <ul>
            <li v-for="(b, i) in deletedBridges" :key="i">
              [{{ b[0][0] }},{{ b[0][1] }}] – [{{ b[1][0] }},{{ b[1][1] }}]
            </li>
          </ul>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import CircleGrid from "./CircleGrid.vue";

export default {
  name: "GameBoard",
  components: { CircleGrid },

  data() {
    return {
      sessionId: null,
      gameState: null,
      validMoves: [],
      aiPlayers: ["blue", "red"],
      lutins: [],
      bridges: [],
      deletedBridges: [],
      validBridgeLocations: [],
      playersOrder: ["green", "blue", "yellow", "red"],
      currentPlayerIndex: 0,
      selectedLutin: null,

      pendingActionType: null,
      pendingMove: null,
      pendingBridge: null,
      pendingPivot: null,
      mustModifyBridge: false,

      highlightIntersections: [],
      feedbackMessage: "",
      feedbackIsError: false,

      chatHistory: [],
      chatQuestion: ""
    };
  },

  mounted() {
    this.startNewGame();
  },

  computed: {
    currentPlayer() {
      return this.playersOrder[this.currentPlayerIndex];
    }
  },

  methods: {
    async handlePontAction({ bridge, pivot }) {
      if (!this.pendingMove) {
        return this.showFeedback("D'abord déplacez un lutin !", true);
      }
      this.pendingBridge = bridge;
      this.pendingPivot  = pivot;
      return this.commitBridgeAction();
    },

    async commitBridgeAction() {
      const bridgeClean = this.pendingBridge.map(pair => pair.slice());
      const pivotClean  = this.pendingPivot ? this.pendingPivot.slice() : null;
      const { fromX, fromY, toX, toY } = this.pendingMove;
      const actionType = this.pendingActionType;

      console.log("[commitBridgeAction] →", {
        fromX, fromY, toX, toY, actionType,
        bridge: bridgeClean,
        pivot:  pivotClean
      });

      try {
        await this.makeMove(fromX, fromY, toX, toY, actionType, bridgeClean, pivotClean);
      } catch (err) {
        return this.showFeedback("Erreur sur l’action pont", true);
      } finally {
        this.pendingMove       = null;
        this.pendingBridge     = null;
        this.pendingPivot      = null;
        this.pendingActionType = null;
        this.mustModifyBridge  = false;
        this.selectedLutin     = null;
      }
    },

    startBridgeAction(type) {
      if (!this.pendingMove) {
        return this.showFeedback("D'abord déplacez un lutin !", true);
      }
      this.pendingActionType = type;
      this.showFeedback(
        type === 'remove'
          ? "Cliquez sur le pont à supprimer"
          : "Cliquez sur le pont à faire pivoter",
        false
      );
    },

    async onLutinClicked(lutin) {
      if (lutin.color !== this.currentPlayer) {
        return this.showFeedback(`Ce n’est pas le tour de ${lutin.color}`, true);
      }
      this.selectedLutin = lutin;
      this.pendingMove = null;
      this.pendingBridge = null;
      this.pendingPivot = null;
      this.mustModifyBridge = false;
      this.highlightIntersections = [];
    },

    onIntersectionClicked(coord) {
      if (!this.selectedLutin) return;

      const ok = this.validMoves.some(m =>
        m.from_x === this.selectedLutin.col &&
        m.from_y === this.selectedLutin.row &&
        m.to_x === coord.x &&
        m.to_y === coord.y
      );
      if (!ok) return this.showFeedback("Coup invalide", true);

      const fromX = this.selectedLutin.col + 1;
      const fromY = this.selectedLutin.row + 1;
      const toX   = coord.x + 1;
      const toY   = coord.y + 1;
      this.pendingMove = { fromX, fromY, toX, toY };
      this.pendingPivot = [fromX, fromY];
      this.mustModifyBridge = true;
    },

    async startNewGame() {
      try {
        const res = await fetch("/api/new_game", { method: "POST", headers: { "Content-Type": "application/json" }, body: JSON.stringify({ mode: "random" }) });
        const data = await res.json();
        this.sessionId = data.session_id;
        if (data.goblins && data.bridges) {
          this.lutins   = data.goblins.map(g => ({ color: g.player, row: g.y-1, col: g.x-1 }));
          this.bridges  = data.bridges.map(b => [[b.y1-1,b.x1-1],[b.y2-1,b.x2-1]]);
        }
        await this.getGameState();
        await this.getValidMoves();
      } catch {
        this.showFeedback("Erreur démarrage partie", true);
      }
    },

    async getGameState() {
      const res = await fetch(`/api/game_state?session_id=${this.sessionId}`);
      const data = await res.json();
      this.gameState = data;
      this.lutins  = (data.goblins||[]).map(g => ({ color: g.player, row: g.y-1, col: g.x-1 }));
      this.bridges = (data.bridges||[]).map(b => [[b.y1-1,b.x1-1],[b.y2-1,b.x2-1]]);
      const cp = (data.current_player||"green").toLowerCase();
      this.currentPlayerIndex = this.playersOrder.indexOf(cp);
    },

    async getValidMoves() {
      const res = await fetch(`/api/valid_moves?session_id=${this.sessionId}`);
      const data = await res.json();
      this.validMoves = (data.valid_moves||[]).map(vm => ({
        ...vm.move,
        from_x: vm.move.from_x-1,
        from_y: vm.move.from_y-1,
        to_x:   vm.move.to_x-1,
        to_y:   vm.move.to_y-1
      }));
    },

    async sendChat() {
      const q = this.chatQuestion.trim();
      if (!q) return;
      // 1) On ajoute le message de l’utilisateur dans l’historique
      this.chatHistory.push({ from: "Vous", text: q });
      this.chatQuestion = "";
      // 2) On interroge l’API
      try {
        const res = await fetch("/api/chat", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ question: q })
        });
        const { reply } = await res.json();
        // 3) On ajoute la réponse du bot
        this.chatHistory.push({ from: "PBot", text: reply });
      } catch (err) {
        console.error("❌ Erreur sendChat:", err);
        this.chatHistory.push({ from: "PBot", text: "Erreur de communication." });
      }
      // 4) Scroll vers le bas
      this.$nextTick(() => {
        const el = this.$el.querySelector(".chat-history");
        if (el) el.scrollTop = el.scrollHeight;
      });
    },


    async makeMove(fx, fy, tx, ty, actionType, bridge, pivot) {
      const body = { session_id: this.sessionId, from_x: fx, from_y: fy, to_x: tx, to_y: ty, action_type: actionType };
      if (bridge) { body.bridge_x1=bridge[0][0]; body.bridge_y1=bridge[0][1]; body.bridge_x2=bridge[1][0]; body.bridge_y2=bridge[1][1]; }
      if (actionType==="rotate" && pivot) { body.pivot_x=pivot[0]; body.pivot_y=pivot[1]; }
      await fetch("/api/make_move", { method:"POST", headers:{"Content-Type":"application/json"}, body:JSON.stringify(body) });
      await this.getGameState();
      await this.getValidMoves();
    },

    showFeedback(msg,isErr=false) {
      this.feedbackMessage=msg; this.feedbackIsError=isErr;
      setTimeout(()=>this.feedbackMessage="",3000);
    }
  }
};
</script>



<style scoped>
/* Container principal : on conserve le flex */
.game-wrapper {
  display: flex;
  gap: 2rem;
  align-items: flex-start;
}

/* Colonne jeu à droite */
.board-column {
  flex: 2;
}

/* styles existants pour .board-container… */
.board-container {
  text-align: center;
  margin: 0 auto;
}


.warning-banner {
  background: #ffcc00;
  color: black;
  font-weight: bold;
  text-align: center;
  padding: 10px;
  margin-top: 15px;
  border-radius: 5px;
}

button.active {
  background: red;
  color: white;
}

@keyframes moveLutin {
  from {
    transform: scale(1.2);
    opacity: 0.7;
  }
  to {
    transform: scale(1);
    opacity: 1;
  }
}

.lutin {
  transition: transform 0.3s ease-in-out;
}

/* 1. Colonne chat : plus large, fond clair, ombre et arrondi */
.chat-column {
  display: flex;
  flex-direction: column;   /* on empile header, history, input */
  height: 100vh;            /* toute la hauteur de la fenêtre */
  box-sizing: border-box;   
  flex: 1.2;                 /* prendre un peu plus d’espace */
  max-width: 420px;          /* largeur maxi */
  background: #ffffff;       /* fond blanc */
  padding: 1.5rem;           /* espace intérieur */
  border-radius: 0.75rem;    /* coins arrondis */
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05);
  display: flex;
  flex-direction: column;
  font-family: 'Segoe UI', sans-serif;
}

/* 2. Titre et intro plus net */
.chat-column h2,
.chat-column p {
  flex: 0 0 auto;           /* header non flexible */
}

/* 3. Historique : fond doux et bulles */
.chat-history {
  flex: 1 1 auto;           /* prend tout l’espace restant */
  overflow-y: auto;         /* scroll interne si overflow */
}
.chat-history .Vous,
.chat-history .PBot {
  max-width: 80%;
  padding: 0.6rem 1rem;
  margin: 0.5rem 0;
  border-radius: 1rem;
  position: relative;
  word-wrap: break-word;
  line-height: 1.4;
  white-space: pre-wrap;
}
.chat-history .Vous {
  margin-left: auto;
  background: #e0f7fa;
  color: #006064;
}
.chat-history .PBot {
  margin-right: auto;
  background: #e8f5e9;
  color: #1b5e20;
}

/* 4. Input : arrondi et hover */

.chat-input {
  flex: 0 0 auto;           /* footer non flexible */
}
.chat-input input {
  flex: 1;
  padding: 0.75rem 1rem;
  border: 1px solid #ccc;
  border-radius: 1.5rem 0 0 1.5rem;
  outline: none;
  transition: border-color 0.2s;
}
.chat-input input:focus {
  border-color: #66afe9;
}
.chat-input button {
  padding: 0.75rem 1.25rem;
  border: none;
  background: #66afe9;
  color: white;
  font-weight: 600;
  border-radius: 0 1.5rem 1.5rem 0;
  cursor: pointer;
  transition: background 0.2s;
}
.chat-input button:hover {
  background: #558acb;
}

/* 5. Petite amélioration de la scrollbar */
.chat-history::-webkit-scrollbar {
  width: 6px;
}
.chat-history::-webkit-scrollbar-thumb {
  background: rgba(0,0,0,0.1);
  border-radius: 3px;
}

</style>
