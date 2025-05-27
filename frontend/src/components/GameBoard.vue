<template>
  <div class="game-wrapper">
    <!-- Colonne de gauche : ChatBot -->
    <div class="chat-column">
      <h2>PBot – Bot explicateur</h2>
      <p>Bonjour, je suis PBot, le bot explicateur du jeu PontuXL.<br/>
         En quoi puis-je vous aider ?</p>

      <div class="chat-history">
        <div
          v-for="(msg, idx) in chatHistory"
          :key="idx"
          :class="msg.from"
        >
          <strong>{{ msg.from }} :</strong>
          <pre class="chat-message">{{ msg.text }}</pre>
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
          :spacing="60"
          :circleRadius="10"
          :lutinRadius="8"
          :lutins="lutins"
          :bridges="bridges"
          :highlightIntersections="highlightIntersections"
          @delete-bridge="onBridgeDelete"
          @rotate-bridge="onBridgeRotate"
          @refresh-game="handleRefresh"
          @lutin-clicked="onLutinClicked"
          @intersection-clicked="onIntersectionClicked"
        />

        <div v-if="mustModifyBridge" class="warning-banner">
          ⚠ Vous devez <b>supprimer</b> ou <b>tourner</b> un pont avant de terminer votre tour !
          <div>
            <button @click="mode = 'delete'" :class="{ active: mode === 'delete' }">
              Supprimer un pont
            </button>
            <button @click="mode = 'rotate'" :class="{ active: mode === 'rotate' }">
              Tourner un pont
            </button>
          </div>
        </div>

        <h3>Tour actuel : {{ currentPlayer }}</h3>
        <h3>Ponts restants ({{ bridges.length }})</h3>

        <div>
          <h4>Ponts supprimés : {{ deletedBridges.length }}</h4>
          <ul>
            <li v-for="(b,i) in deletedBridges" :key="i">
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
      aiPlayers: ["blue","red"],
      lutins: [],
      bridges: [],
      deletedBridges: [],
      validBridgeLocations: [],
      playersOrder: ["green", "blue", "yellow", "red"],
      currentPlayerIndex: 0,
      selectedLutin: null,
      mustModifyBridge: false,
      mode: '',
      highlightIntersections: [],
      feedbackMessage: "",
      feedbackIsError: false,
      chatInput: '',
    

      // --- ChatBot intégré ---
      chatHistory: [],      // { from: "Vous"|"Bot", text: String }
      chatQuestion: ""      // ce que l'utilisateur saisit
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
      this.handleBridgeAction("remove", bridge);
    },
    onBridgeRotate(bridge) {
      const pivot = bridge[0];
      this.handleBridgeAction("rotate", bridge, pivot);
    },
    async handleRefresh() {
      await this.getGameState();
      await this.getValidMoves();
    },
    async startNewGame() {
      try {
        const response = await fetch('/api/new_game', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ mode: 'random' })
        });
        const data = await response.json();
        this.sessionId = data.session_id;
        if (data.goblins && data.bridges) {
          this.lutins = data.goblins.map(g => ({
            color: g.player, row: g.y - 1, col: g.x - 1
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
      } catch (error) {
        console.error("❌ Erreur dans fetch new_game :", error);
      }
    },
    async getGameState() {
      const res = await fetch(`/api/game_state?session_id=${this.sessionId}`);
      const data = await res.json();
      this.gameState = data;
      // On conserve la sélection si possible
      let prevSelected = this.selectedLutin;
      this.lutins = (data.goblins||[]).map(g => ({
        color: g.player, row: g.y - 1, col: g.x - 1
      }));
      this.bridges = (data.bridges || []).map(b => [
        [b.y1 - 1, b.x1 - 1],
        [b.y2 - 1, b.x2 - 1]
      ]);
      this.currentPlayerIndex = this.playersOrder.indexOf(data.current_player||"green");
      // Recherche du lutin sélectionné dans la nouvelle liste
      if (prevSelected) {
        const found = this.lutins.find(l => l.color === prevSelected.color && l.row === prevSelected.row && l.col === prevSelected.col);
        this.selectedLutin = found || null;
      }
      console.log("Lutins reçus :", this.lutins, "Selected:", this.selectedLutin);
    },
    async getValidMoves() {
      const res = await fetch(`/api/valid_moves?session_id=${this.sessionId}`);
      const data = await res.json();
      this.validMoves = (data.valid_moves || []).map(vm => ({
        ...vm.move,
        from_x: vm.move.from_x - 1,
        from_y: vm.move.from_y - 1,
        to_x: vm.move.to_x - 1,
        to_y: vm.move.to_y - 1
      }));
      if (this.validMoves.length > 0) {
        console.log('[DEBUG] Exemple de validMove:', this.validMoves[0]);
      }
    },
    async makeMove(fromX, fromY, toX, toY, actionType="none", bridge=null, pivot=null) {
      console.log('[DEBUG] makeMove', {fromX, fromY, toX, toY, actionType, bridge, pivot});
      try {
        const body = { session_id:this.sessionId,
                       from_x:fromX, from_y:fromY,
                       to_x:toX, to_y:toY,
                       action_type:actionType };
        if (bridge) {
          body.bridge_x1=bridge[0][0]; body.bridge_y1=bridge[0][1];
          body.bridge_x2=bridge[1][0]; body.bridge_y2=bridge[1][1];
        }
        if (actionType==="rotate" && pivot) {
          body.pivot_x=pivot[0]; body.pivot_y=pivot[1];
        }
        await fetch('/api/make_move', {
          method:"POST",
          headers:{"Content-Type":"application/json"},
          body:JSON.stringify(body)
        });
        await this.getGameState();
        await this.getValidMoves();
        // si le joueur suivant est une IA, on la déclenche
        if (this.aiPlayers.includes(this.currentPlayer)) {
          await this.getAIMove();
        }
      } catch (error) {
        console.error("Erreur makeMove :", error);
        this.showFeedback("Erreur lors du mouvement", true);
      }
    },
    async getAIMove() {
      try {
        await fetch(`/api/ai_move?session_id=${this.sessionId}`);
        await this.getGameState();
        await this.getValidMoves();
      } catch (error) {
        console.error("Erreur IA :", error);
      }
    },
    onLutinClicked(lutin) {
      console.log('[DEBUG] CLIC LUTIN', lutin);
      if (lutin.color !== this.currentPlayer) {
        this.showFeedback(`Ce n’est pas le tour de ${lutin.color}`, true);
        return;
      }
      this.selectedLutin = lutin;
      this.mustModifyBridge = false;
      this.highlightIntersections = [];
    },
    async onIntersectionClicked(coord) {
      console.log('[DEBUG] CLIC INTERSECTION', coord, 'selectedLutin:', this.selectedLutin);
      if (!this.selectedLutin) return;
      const isValid = this.validMoves.some(move =>
        move.from_x===this.selectedLutin.col &&
        move.from_y===this.selectedLutin.row &&
        move.to_x  ===coord.x &&
        move.to_y  ===coord.y
      );
      if (!isValid) {
        console.warn('[DEBUG] Coup non trouvé. validMoves:', this.validMoves, 'Comparé à:', {
          from_x: this.selectedLutin.col,
          from_y: this.selectedLutin.row,
          to_x: coord.x,
          to_y: coord.y
        });
        this.showFeedback("Coup invalide", true);
        return;
      }
      await this.makeMove(
        this.selectedLutin.col + 1, this.selectedLutin.row + 1,
        coord.x + 1, coord.y + 1
      );
      this.selectedLutin = null;
    },
    showFeedback(msg, isErr=false) {
      this.feedbackMessage=msg; this.feedbackIsError=isErr;
      setTimeout(() => this.feedbackMessage="", 3000);
    },
    generateBridgesAndLocations() {
      const allLocations = [];
      for (let r=0; r<6; r++){
        for (let c=0; c<6; c++){
          if(c<5) allLocations.push([[r,c],[r,c+1]]);
          if(r<5) allLocations.push([[r,c],[r+1,c]]);
        }
      }
      return { allLocations };
    },
    async handleBridgeAction(type, bridge, pivot=null) {
      if (!this.selectedLutin) {
        this.showFeedback("Déplacez un lutin d'abord !", true);
        return;
      }
      try {
        await this.makeMove(
          this.selectedLutin.col + 1, this.selectedLutin.row + 1,
          this.selectedLutin.col + 1, this.selectedLutin.row + 1,
          type, bridge, pivot
        );
        this.selectedLutin=null; this.mustModifyBridge=false;
      } catch(e) {
        console.error("Erreur pont :", e);
        this.showFeedback("Erreur lors de l'action sur le pont", true);
      }
    },
    nextPlayer() {
      this.currentPlayerIndex = (this.currentPlayerIndex+1) % this.playersOrder.length;
      this.mode = null;
    },

    // ----- Partie Chat -----
    /** Envoie la question et ajoute dans l'historique */
    async sendChat() {
      const q = this.chatQuestion.trim();
      if (!q) return;
      this.chatHistory.push({ from:"Vous", text:q });
      this.chatQuestion = "";
      try {
        const res = await fetch("/api/chat", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ question:q })
        });
        const { reply } = await res.json();
        this.chatHistory.push({ from:"PBot", text:reply });
      } catch(err) {
        console.error("Erreur chat :", err);
       this.chatHistory.push({ from:"PBot", text:"Erreur de communication." });
      }
      // scroll bas
      this.$nextTick(() => {
        const el = this.$el.querySelector(".chat-history");
        if (el) el.scrollTop = el.scrollHeight;
      });
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

/* Colonne chat à gauche */
.chat-column {
  flex: 1;
  max-width: 350px;      /* un peu plus large */
  font-family: monospace;
  font-size: 16px;       /* police plus grande */
  line-height: 1.4;
}

/* Titre et introduction */
.chat-column h2 {
  margin-bottom: 0.5rem;
  font-size: 1.2em;
}
.chat-column p {
  margin-bottom: 1rem;
  white-space: pre-wrap;
  font-size: 1em;
}

/* Historique du chat */
.chat-history {
  max-height: 500px;     /* plus de hauteur */
  overflow-y: auto;
  border: 1px solid #ccc;
  padding: 1rem;
  background: #fafafa;
  font-size: 1em;
}
.chat-history .Vous,
.chat-history .PBot {
  margin: 0.75rem 0;
}
.chat-history .Vous {
  text-align: right;
  color: #0055aa;        /* bleu un peu plus foncé */
}
.chat-history .PBot {
  text-align: left;
  color: #007700;        /* vert plus profond */
}
.chat-message pre {
  margin: 0.25rem 0 0 0;
  white-space: pre-wrap;
  font-family: monospace;
}

/* Zone de saisie chat */
.chat-input {
  display: flex;
  margin-top: 1rem;
}
.chat-input input {
  flex: 1;
  padding: 0.6rem;
  font-size: 1em;
  border: 1px solid #888;
  border-radius: 4px 0 0 4px;
}
.chat-input button {
  padding: 0.6rem 1.2rem;
  font-size: 1em;
  border: 1px solid #888;
  border-left: 0;
  background: #ddd;
  cursor: pointer;
  border-radius: 0 4px 4px 0;
}

/* Colonne jeu à droite */
.board-column {
  flex: 2;
}

/* Vos styles existants pour .board-container… */
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
</style>
