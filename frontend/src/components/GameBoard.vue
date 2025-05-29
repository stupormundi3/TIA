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

        <!-- Feedback général -->
        <div
          v-if="feedbackMessage"
          class="feedback-banner"
          :class="{ error: feedbackIsError }"
        >
          {{ feedbackMessage }}
        </div>

        <!-- Grille du jeu -->
        <CircleGrid
          :rows="6"
          :cols="6"
          :spacing="60"
          :circleRadius="10"
          :lutinRadius="8"
          :lutins="lutins"
          :bridges="bridges"
          :highlightIntersections="highlightIntersections"
          @refresh-game="handleRefresh"
          @lutin-clicked="onLutinClicked"
          @intersection-clicked="onIntersectionClicked"
          @delete-bridge="onBridgeDelete"
          @rotate-bridge="onBridgeRotate"
        />

        <!-- Choix d’action sur le pont -->
       <div v-if="mustModifyBridge" class="warning-banner">
        ⚠ Vous devez <b>supprimer</b> ou <b>tourner</b> un pont avant de terminer votre tour !
        <div>
          <button @click="handleBridgeAction('remove')">
            Supprimer un pont
          </button>
          <button @click="handleBridgeAction('rotate')">
            Tourner un pont
          </button>
        </div>
      </div>

        <!-- Statistiques de la partie -->
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
      // --- État du jeu ---
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

      // phase de déplacement + action pont
      pendingMove: null,
      pendingBridge: null,
      pendingPivot: null,
      mustModifyBridge: false,

      // interface
      highlightIntersections: [],
      feedbackMessage: "",
      feedbackIsError: false,

      // ChatBot
      chatHistory: [],      // { from:"Vous"|"PBot", text: String }
      chatQuestion: ""      // texte saisi
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
    // ----- Partie jeu -----
    onBridgeDelete(bridge) {
      if (!this.mustModifyBridge) return;
      this.pendingBridge = bridge;
      this.handleBridgeAction("remove");
    },

    onBridgeRotate(bridge) {
      if (!this.mustModifyBridge) return;
      const pivot = bridge[0];
      this.pendingBridge = bridge;
      this.pendingPivot = pivot;
      this.handleBridgeAction("rotate");
    },

    async handleRefresh() {
      await this.getGameState();
      await this.getValidMoves();
    },

    async startNewGame() {
      try {
        const res = await fetch("/api/new_game", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ mode: "random" })
        });
        const data = await res.json();
        this.sessionId = data.session_id;
        if (data.goblins && data.bridges) {
          this.lutins = data.goblins.map(g => ({
            color: g.player,
            row: g.y - 1,
            col: g.x - 1
          }));
          this.bridges = data.bridges.map(b => [
            [b.y1 - 1, b.x1 - 1],
            [b.y2 - 1, b.x2 - 1]
          ]);
        }
        await this.getGameState();
        await this.getValidMoves();
        const { allLocations } = this.generateBridgesAndLocations();
        this.validBridgeLocations = allLocations;
      } catch (err) {
        console.error("❌ Erreur startNewGame:", err);
        this.showFeedback("Erreur lors du démarrage de la partie", true);
      }
    },

    async getGameState() {
      try {
        const res = await fetch(`/api/game_state?session_id=${this.sessionId}`);
        const data = await res.json();
        this.gameState = data;
        const prev = this.selectedLutin;
        this.lutins = (data.goblins || []).map(g => ({
          color: g.player,
          row: g.y - 1,
          col: g.x - 1
        }));
        this.bridges = (data.bridges || []).map(b => [
          [b.y1 - 1, b.x1 - 1],
          [b.y2 - 1, b.x2 - 1]
        ]);
        this.currentPlayerIndex = this.playersOrder.indexOf(data.current_player || "green");
        // restore selection
        if (prev) {
          const found = this.lutins.find(
            l => l.color === prev.color && l.row === prev.row && l.col === prev.col
          );
          this.selectedLutin = found || null;
        }
      } catch (err) {
        console.error("❌ Erreur getGameState:", err);
        this.showFeedback("Erreur récupération état", true);
      }
    },

    async getValidMoves() {
      try {
        const res = await fetch(`/api/valid_moves?session_id=${this.sessionId}`);
        const data = await res.json();
        this.validMoves = (data.valid_moves || []).map(vm => ({
          ...vm.move,
          from_x: vm.move.from_x - 1,
          from_y: vm.move.from_y - 1,
          to_x: vm.move.to_x - 1,
          to_y: vm.move.to_y - 1
        }));
      } catch (err) {
        console.error("❌ Erreur getValidMoves:", err);
        this.showFeedback("Erreur récupération coups valides", true);
      }
    },

    async makeMove(fromX, fromY, toX, toY, actionType = "none", bridge = null, pivot = null) {
      try {
        const body = {
          session_id: this.sessionId,
          from_x: fromX,
          from_y: fromY,
          to_x: toX,
          to_y: toY,
          action_type: actionType
        };
        if (bridge) {
          body.bridge_x1 = bridge[0][0];
          body.bridge_y1 = bridge[0][1];
          body.bridge_x2 = bridge[1][0];
          body.bridge_y2 = bridge[1][1];
        }
        if (actionType === "rotate" && pivot) {
          body.pivot_x = pivot[0];
          body.pivot_y = pivot[1];
        }
        await fetch("/api/make_move", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify(body)
        });
        // rafraîchir l’état après coup
        await this.getGameState();
        await this.getValidMoves();
        // IA si nécessaire
        if (this.aiPlayers.includes(this.currentPlayer)) {
          await this.getAIMove();
        }
      } catch (err) {
        console.error("❌ Erreur makeMove:", err);
        this.showFeedback("Erreur lors du mouvement", true);
      }
    },

    async getAIMove() {
      try {
        await fetch(`/api/ai_move?session_id=${this.sessionId}`);
        await this.getGameState();
        await this.getValidMoves();
      } catch (err) {
        console.error("❌ Erreur getAIMove:", err);
        this.showFeedback("Erreur IA", true);
      }
    },

    onLutinClicked(lutin) {
      console.log("[DEBUG] CLIC LUTIN", lutin);
      if (lutin.color !== this.currentPlayer) {
        this.showFeedback(`Ce n’est pas le tour de ${lutin.color}`, true);
        return;
      }
      // reset phase déplacement/action
      this.selectedLutin = lutin;
      this.pendingMove = null;
      this.pendingBridge = null;
      this.pendingPivot = null;
      this.mustModifyBridge = false;
      this.highlightIntersections = [];
    },

    onIntersectionClicked(coord) {
      if (!this.selectedLutin) return;
      // vérifier validité
      const ok = this.validMoves.some(m =>
        m.from_x === this.selectedLutin.col &&
        m.from_y === this.selectedLutin.row &&
        m.to_x === coord.x &&
        m.to_y === coord.y
      );
      if (!ok) {
        this.showFeedback("Coup invalide", true);
        return;
      }
      // stocker move en 1-based
      const fromX = this.selectedLutin.col + 1;
      const fromY = this.selectedLutin.row + 1;
      const toX = coord.x + 1;
      const toY = coord.y + 1;
      this.pendingMove = { fromX, fromY, toX, toY };

      // pont traversé
      this.pendingBridge = [
        [fromX, fromY],
        [toX, toY]
      ];
      this.pendingPivot = [fromX, fromY];

      this.mustModifyBridge = true;
    },

    async handleBridgeAction(type, bridge = null, pivot = null) {
      if (!this.pendingMove) {
        this.showFeedback("Sélectionnez d'abord un déplacement !", true);
        return;
      }
      // utiliser valeurs stockées si pas passées en param
      bridge = bridge || this.pendingBridge;
      pivot = pivot || this.pendingPivot;

      const { fromX, fromY, toX, toY } = this.pendingMove;
      try {
        await this.makeMove(fromX, fromY, toX, toY, type, bridge, pivot);
        // reset phase déplacement/action
        this.pendingMove = null;
        this.pendingBridge = null;
        this.pendingPivot = null;
        this.mustModifyBridge = false;
        this.selectedLutin = null;
      } catch (err) {
        console.error("❌ Erreur handleBridgeAction:", err);
        this.showFeedback("Erreur lors de l'action sur le pont", true);
      }
    },

    nextPlayer() {
      this.currentPlayerIndex =
        (this.currentPlayerIndex + 1) % this.playersOrder.length;
    },

    // ----- Partie Chat -----
    async sendChat() {
      const q = this.chatQuestion.trim();
      if (!q) return;
      this.chatHistory.push({ from: "Vous", text: q });
      this.chatQuestion = "";
      try {
        const res = await fetch("/api/chat", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ question: q })
        });
        const { reply } = await res.json();
        this.chatHistory.push({ from: "PBot", text: reply });
      } catch (err) {
        console.error("❌ Erreur sendChat:", err);
        this.chatHistory.push({
          from: "PBot",
          text: "Erreur de communication."
        });
      }
      this.$nextTick(() => {
        const el = this.$el.querySelector(".chat-history");
        if (el) el.scrollTop = el.scrollHeight;
      });
    },

    generateBridgesAndLocations() {
      const allLocations = [];
      for (let r = 0; r < 6; r++) {
        for (let c = 0; c < 6; c++) {
          if (c < 5) allLocations.push([[r, c], [r, c + 1]]);
          if (r < 5) allLocations.push([[r, c], [r + 1, c]]);
        }
      }
      return { allLocations };
    },

    showFeedback(msg, isErr = false) {
      this.feedbackMessage = msg;
      this.feedbackIsError = isErr;
      setTimeout(() => (this.feedbackMessage = ""), 3000);
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
