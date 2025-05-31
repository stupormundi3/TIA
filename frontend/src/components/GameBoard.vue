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

        <!-- feedbackMessage utilise la classe warning-banner -->
        <div
          v-if="feedbackMessage"
          class="warning-banner"
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
              {{ b.actor }} a {{ b.action }} le pont [{{ b.bridge[0][0] }},{{ b.bridge[0][1] }}] – [{{ b.bridge[1][0] }},{{ b.bridge[1][1] }}]
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
      highlightBridges: [],
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
      console.log('[handlePontAction] bridge:', bridge, 'pivot:', pivot);
      if (!this.pendingMove) {
        return this.showFeedback("D'abord déplacez un lutin !", true);
      }
      this.pendingBridge = bridge;
      this.pendingPivot  = pivot;
      return this.commitBridgeAction();
    },

    async commitBridgeAction() {
  console.log('[commitBridgeAction] pendingMove, pendingBridge, pendingPivot:', this.pendingMove, this.pendingBridge, this.pendingPivot);
  const bridgeClean = this.pendingBridge.map(pair => pair.slice());
  const pivotClean  = this.pendingPivot ? this.pendingPivot.slice() : null;
  const { fromX, fromY, toX, toY } = this.pendingMove;
  const actionType = this.pendingActionType;

  // --- On capture la couleur du joueur AVANT l’appel au backend ---
  const actorBeforeMove = this.currentPlayer;

  try {
    await this.makeMove(fromX, fromY, toX, toY, actionType, bridgeClean, pivotClean);
    console.log('[commitBridgeAction] action performed:', actionType);
    if (actionType === 'remove') {
      this.deletedBridges.push({
        bridge: bridgeClean,
        actor: `${actorBeforeMove} (joueur)`,
        action: 'supprimé'
      });
    } else if (actionType === 'rotate') {
      this.deletedBridges.push({
        bridge: bridgeClean,
        actor: `${actorBeforeMove} (joueur)`,
        action: 'tourné'
      });
    }
    // Feedback utilisateur
    let msg = `Vous avez déplacé le lutin de (${fromX},${fromY}) à (${toX},${toY})`;
    if (actionType === 'remove') {
      msg += ` et supprimé le pont [${bridgeClean[0][0]},${bridgeClean[0][1]}]–[${bridgeClean[1][0]},${bridgeClean[1][1]}]`;
    } else if (actionType === 'rotate') {
      msg += ` et tourné le pont [${bridgeClean[0][0]},${bridgeClean[0][1]}]–[${bridgeClean[1][0]},${bridgeClean[1][1]}] autour du pivot (${pivotClean[0]},${pivotClean[1]})`;
    }
    this.showFeedback(msg, false);

    // Coup IA si suivant
    if (this.aiPlayers.includes(this.currentPlayer)) {
      await this.handleAIMove();
    }
  } catch (err) {
    this.showFeedback("Erreur sur l’action pont", true);
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
        this.showFeedback("Erreur démarrage partie Connecte le backend", true);
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
      // Conserve coups valides avec action
      this.validMoves = (data.valid_moves||[]).map(vm => ({
        ...vm.move,
        action: vm.action,
        from_x: vm.move.from_x-1,
        from_y: vm.move.from_y-1,
        to_x:   vm.move.to_x-1,
        to_y:   vm.move.to_y-1
      }));
      // Détermine les ponts disponibles pour rotation (0-based coords)
      this.validBridgeLocations = this.validMoves
        .filter(m => m.action.type === 'rotate')
        .map(m => [ [m.action.y1-1, m.action.x1-1], [m.action.y2-1, m.action.x2-1] ]);
      // Élimination si ce joueur n’a plus de coups en phase de jeu
      if (data.phase === 'play' && this.validMoves.length === 0) {
        const eliminated = this.currentPlayer;
        console.log(`[getValidMoves] ${eliminated} éliminé (plus de coups valides)`);
        this.showFeedback(`Le joueur ${eliminated} est éliminé`, false);
        // Retirer le joueur et ajuster l’index
        this.playersOrder = this.playersOrder.filter(c => c !== eliminated);
        this.currentPlayerIndex = this.currentPlayerIndex % this.playersOrder.length;
        // Relancer pour le joueur suivant
        return this.getValidMoves();
      }
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
      console.log('[makeMove] deploying move', { fx, fy, tx, ty, actionType, bridge, pivot });
      const body = { session_id: this.sessionId, from_x: fx, from_y: fy, to_x: tx, to_y: ty, action_type: actionType };
      if (bridge) { body.bridge_x1=bridge[0][0]; body.bridge_y1=bridge[0][1]; body.bridge_x2=bridge[1][0]; body.bridge_y2=bridge[1][1]; }
      if (actionType==="rotate" && pivot) { body.pivot_x=pivot[0]; body.pivot_y=pivot[1]; }
      await fetch("/api/make_move", { method:"POST", headers:{"Content-Type":"application/json"}, body:JSON.stringify(body) });
      await this.getGameState();
      await this.getValidMoves();
    },

    showFeedback(msg,isErr=false) {
      this.feedbackMessage=msg; this.feedbackIsError=isErr;
      setTimeout(()=>this.feedbackMessage="",40000);
    },

    // IA: jouer automatiquement
    async handleAIMove() {
      const aiColor = this.currentPlayer; // mémorise la couleur IA avant qu’elle ne change
      this.showFeedback(`IA (${aiColor}) réfléchit…`, false);
      try {
        const res = await fetch(`/api/ai_move?session_id=${this.sessionId}`);
        const data = await res.json();
        console.log('[handleAIMove] response from backend:', data);
        const { move, action } = data;
        console.log('[handleAIMove] action type:', action?.type);
        if (data.status !== 'success') {
          return this.showFeedback('Erreur IA', true);
        }
        const fx = move.from_x; const fy = move.from_y;
        const tx = move.to_x;   const ty = move.to_y;
        let bridge = null;
        let pivot  = null;
        if (action && (action.type === 'remove' || action.type === 'rotate')) {
          bridge = [[action.x1, action.y1], [action.x2, action.y2]];
          if (action.type === 'rotate') {
            pivot = [action.pivot_x, action.pivot_y];
          }
        }
        await this.makeMove(fx, fy, tx, ty, action?.type, bridge, pivot);
        if (action?.type === 'remove') {
          const removedBridge = [
            [action.x1, action.y1],
            [action.x2, action.y2]
          ];
          this.deletedBridges.push({
            bridge: removedBridge,
            actor: `IA (${aiColor})`,
            action: 'supprimé'
          });
        } else if (action?.type === 'rotate') {
          const rotatedBridge = [
            [action.x1, action.y1],
            [action.x2, action.y2]
          ];
          this.deletedBridges.push({
            bridge: rotatedBridge,
            actor: `IA (${aiColor})`,
            action: 'tourné'
          });
        }
        // Vérification des données reçues pour éviter les incohérences
        console.log('[handleAIMove] Vérification des données:', { action, move });
        // Feedback IA avec couleur mémorisée
        let msgIA = `IA (${aiColor}) a déplacé de (${fx},${fy}) à (${tx},${ty})`;
        if (action?.type === 'remove') {
          msgIA += ` et supprimé le pont [${action.x1},${action.y1}]-[${action.x2},${action.y2}]`;
        } else if (action?.type === 'rotate') {
          msgIA += ` et tourné le pont [${action.x1},${action.y1}]-[${action.x2},${action.y2}] autour du pivot (${action.pivot_x},${action.pivot_y})`;
        }
        this.showFeedback(msgIA, false);
      } catch (err) {
        this.showFeedback('Erreur IA', true);
        console.error('[handleAIMove] error:', err);
      }
    }
  }
};
</script>

<style scoped>
.game-wrapper {
  display: flex;
  height: 100vh;
}

.chat-column {
  background-color: #f4f4f9;
  border-right: 1px solid #ddd;
  padding: 20px;
  width: 300px;
  display: flex;
  flex-direction: column;
}

.chat-history {
  flex-grow: 1;
  overflow-y: auto;
  margin-bottom: 10px;
}

.chat-message {
  background-color: #e1ffc7;
  border-radius: 5px;
  margin: 5px 0;
  padding: 10px;
}

.chat-input {
  display: flex;
}

.chat-input input {
  border: 1px solid #ccc;
  border-radius: 4px;
  flex-grow: 1;
  margin-right: 10px;
  padding: 10px;
}

.chat-input button {
  background-color: #007bff;
  border: none;
  border-radius: 4px;
  color: white;
  cursor: pointer;
  padding: 10px 20px;
}

.chat-input button:hover {
  background-color: #0056b3;
}

.board-column {
  flex-grow: 1;
  padding: 20px;
}

.warning-banner {
  background-color: #fff3cd;
  border: 1px solid #ffeeba;
  border-radius: 4px;
  color: #856404;
  margin-bottom: 10px;
  padding: 10px;
}

h2, h3, h4 {
  color: #333;
}

h2 {
  border-bottom: 2px solid #007bff;
  padding-bottom: 10px;
}

h3 {
  margin-top: 20px;
}

ul {
  padding-left: 20px;
}

button {
  background-color: #28a745;
  border: none;
  border-radius: 4px;
  color: white;
  cursor: pointer;
  margin-right: 10px;
  padding: 10px 15px;
}

button:hover {
  background-color: #218838;
}
</style>
