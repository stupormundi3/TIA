<!-- GameBoard.vue -->
<template>
  <div class="board-container">
    <h2>PontuXL - 6x6</h2>
    
    <!-- Feedback visuel (succès / erreur) -->
    <div v-if="feedbackMessage" class="feedback-banner" :class="{ error: feedbackIsError }">
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
      
      @lutin-clicked="onLutinClicked"
      @intersection-clicked="onIntersectionClicked"

      @delete-bridge="handleBridgeAction"   
      @rotate-bridge="handleBridgeAction"  
    />

    <!-- Obligation de supprimer ou tourner un pont -->
    <div v-if="mustModifyBridge" class="warning-banner">
      <p>⚠ Vous devez <b>supprimer</b> ou <b>tourner</b> un pont avant de terminer votre tour !</p>
      <button @click="mode = 'delete'" :class="{ active: mode === 'delete' }">
        Supprimer un pont
      </button>
      <button @click="mode = 'rotate'" :class="{ active: mode === 'rotate' }">
        Tourner un pont
      </button>
    </div>

    <h3>Tour actuel : {{ currentPlayer }}</h3>
    <h3>Ponts restants ({{ bridges.length }})</h3>

    <!-- Liste des ponts supprimés -->
    <div>
      <h4>Ponts supprimés : {{ deletedBridges.length }}</h4>
      <ul>
        <li v-for="(b,i) in deletedBridges" :key="i">
          [{{ b[0][0] }},{{ b[0][1] }}] - [{{ b[1][0] }},{{ b[1][1] }}]
        </li>
      </ul>
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
      /* Données du jeu */
      lutins: [
        // Ex. 16 lutins, 4 par couleur
        { color: "green", row: 0, col: 0 },
        { color: "green", row: 0, col: 2 },
        { color: "green", row: 1, col: 1 },
        { color: "green", row: 2, col: 0 },

        { color: "blue", row: 0, col: 5 },
        { color: "blue", row: 1, col: 4 },
        { color: "blue", row: 3, col: 1 },
        { color: "blue", row: 4, col: 2 },

        { color: "yellow", row: 1, col: 3 },
        { color: "yellow", row: 2, col: 2 },
        { color: "yellow", row: 4, col: 4 },
        { color: "yellow", row: 5, col: 1 },

        { color: "red", row: 2, col: 5 },
        { color: "red", row: 3, col: 5 },
        { color: "red", row: 4, col: 5 },
        { color: "red", row: 5, col: 4 },
      ],

      bridges: [],            // Ponts actifs sur le plateau
      deletedBridges: [],     // Historique des ponts supprimés

      validBridgeLocations: [], // Tous les emplacements initiaux possibles

      // Gestion des joueurs
      playersOrder: ["green", "blue", "yellow", "red"],
      currentPlayerIndex: 0,

      // Logique de déplacement/édition
      selectedLutin: null,
      mustModifyBridge: false,
      mode: null, // "delete" ou "rotate"
      selectedBridgeToRotate: null,
      pivot: null, // [row, col] pivot pour la rotation

      // Surbrillance d’intersections
      highlightIntersections: [],

      // Feedback
      feedbackMessage: "",
      feedbackIsError: false
    };
  },

  created() {
    
    // Générer ponts initiaux + emplacements valides
    const { currentBridges, allLocations } = this.generateBridgesAndLocations();
    this.bridges = currentBridges;
    this.validBridgeLocations = allLocations;
    
  },

  computed: {
    currentPlayer() {
      return this.playersOrder[this.currentPlayerIndex];
    }
  },

  methods: {
    /* =========================
       Génération des ponts initiaux
       ========================= */
    generateBridgesAndLocations() {
      const currentBridges = [];
      const allLocations = [];
      for (let row = 0; row < 6; row++) {
        for (let col = 0; col < 6; col++) {
          if (col < 5) {
            const b = [[row, col], [row, col + 1]];
            currentBridges.push(b);
            allLocations.push(b);
          }
          if (row < 5) {
            const b = [[row, col], [row + 1, col]];
            currentBridges.push(b);
            allLocations.push(b);
          }
        }
      }
      return { currentBridges, allLocations };
    },

    /* =========================
       Feedback visuel
       ========================= */
    showFeedback(msg, isErr=false) {
      this.feedbackMessage = msg;
      this.feedbackIsError = isErr;
      setTimeout(() => {
        this.feedbackMessage = "";
      }, 3000);
    },

    /* =========================
       Animation du déplacement
       ========================= */
    animateMove(lutin, row, col) {
      if (!lutin) return;
      lutin.animated = true;
      setTimeout(() => {
        lutin.row = row;
        lutin.col = col;
        lutin.animated = false;
      }, 300);
    },

    /* =========================
       Clic sur un lutin
       ========================= */
    onLutinClicked(lutin) {
      if (lutin.color !== this.currentPlayer) {
        this.showFeedback(`Ce n’est pas le tour de ${lutin.color}`, true);
        return;
      }
      this.selectedLutin = lutin;
      this.mustModifyBridge = false;
      this.highlightIntersections = [];
    },

    /* =========================
       Clic sur une intersection
       ========================= */
    onIntersectionClicked(row, col) {
      // Cas rotation (2ᵉ clic)
      if (this.mode === "rotate" && this.selectedBridgeToRotate && this.pivot) {
        this.finishBridgeRotation(row, col);
        return;
      }

      // Sinon, déplacement normal d'un lutin
      if (!this.selectedLutin) return;

      if (!this.isMoveValid(this.selectedLutin, row, col)) {
        this.showFeedback("Déplacement invalide !", true);
        return;
      }
      this.animateMove(this.selectedLutin, row, col);
      this.mustModifyBridge = true; // On doit obligatoirement modifier un pont ensuite
    },

    /* =========================
       Vérification du déplacement
       ========================= */
    isMoveValid(lutin, nr, nc) {
      if (nr < 0 || nr > 5 || nc < 0 || nc > 5) return false;
      if (!this.hasBridge(lutin.row, lutin.col, nr, nc)) return false;
      const occupant = this.lutins.find(x => x.row === nr && x.col === nc);
      return !occupant;
    },

    /* =========================
       Test d’existence d’un pont
       ========================= */
    hasBridge(r1, c1, r2, c2) {
      return this.bridges.some(
        b =>
          (b[0][0] === r1 && b[0][1] === c1 && b[1][0] === r2 && b[1][1] === c2) ||
          (b[1][0] === r1 && b[1][1] === c1 && b[0][0] === r2 && b[0][1] === c2)
      );
    },

    /* =========================
       Gérer l’action sur un pont
       ========================= */
    handleBridgeAction(bridge) {
      if (!this.mustModifyBridge) {
        this.showFeedback("Vous devez d'abord déplacer un lutin !", true);
        return;
      }
      if (this.mode === "delete") {
        this.removeBridge(bridge);
      } else if (this.mode === "rotate") {
        this.rotateBridge(bridge);
      } else {
        this.showFeedback("Choisissez 'supprimer' ou 'tourner' !",true);
      }
    },

    /* =========================
       Supprimer un pont
       ========================= */
       removeBridge(bridge) {
    console.log("[DEBUG] ❌ Suppression du pont:", bridge);
    console.log("[DEBUG] 📌 Avant suppression, bridges =", this.bridges);

    // On stocke dans deletedBridges pour historique
    this.deletedBridges.push(bridge);

    // On le retire du tableau principal
    this.bridges = this.bridges.filter(b =>
        !(
            b[0][0] === bridge[0][0] && b[0][1] === bridge[0][1] &&
            b[1][0] === bridge[1][0] && b[1][1] === bridge[1][1]
        )
    );

    console.log("[DEBUG] 🔥 Après suppression, bridges =", this.bridges);
    this.mustModifyBridge = false;
    this.nextPlayer();
}
,

    /* =========================
       Rotation : 1er clic => pivot
                  2ᵉ clic => ou ?
       ========================= */
       rotateBridge(bridge) {
    console.log("[ROTATE] 1er clic sur le pont :", bridge);
    if (!this.mustModifyBridge) {
        this.showFeedback("Déplacez un lutin d'abord !", true);
        return;
    }

    if (!this.selectedBridgeToRotate) {
        console.log("[DEBUG] 🎯 Sélection du pivot :", bridge[0]);
        this.selectedBridgeToRotate = bridge;
        this.pivot = bridge[0];
        this.showFeedback(
          `Pivot : (${this.pivot[0]},${this.pivot[1]}). Cliquez sur l’intersection où placer l’autre extrémité.`,
          false
        );
        this.updateHighlightIntersections();
        return;
    }

    console.log("[DEBUG] ⚠️ Pivot déjà sélectionné :", this.pivot);
}
,

    

    // Met à jour la liste `highlightIntersections` pour la surbrillance
    updateHighlightIntersections() {
  console.log("[DEBUG] 📌 Liste des emplacements valides dans validBridgeLocations:", this.validBridgeLocations);

  this.highlightIntersections = [];
  const [px, py] = this.pivot; // pivot

  console.log(`[DEBUG] 🎯 Vérification des intersections valides pour pivot (${px}, ${py})`);

  // Liste des déplacements possibles (haut, bas, gauche, droite)
  const directions = [
    [-1, 0], // Haut
    [1, 0],  // Bas
    [0, -1], // Gauche
    [0, 1]   // Droite
  ];

  for (let [dx, dy] of directions) {
    let row = px + dx;
    let col = py + dy;

    // Vérifier si la position est dans les limites du plateau
    if (row < 0 || row > 5 || col < 0 || col > 5) continue;

    // Vérifie si le pont [pivot -> (row, col)] est valide
    const foundInValidLocations = this.validBridgeLocations.some(b =>
      ((b[0][0] === px && b[0][1] === py && b[1][0] === row && b[1][1] === col) ||
       (b[1][0] === px && b[1][1] === py && b[0][0] === row && b[0][1] === col))
    );

    // Vérifie si un pont existe déjà (sans compter celui qu'on tourne)
    const alreadyExists = this.bridges.some(b =>
      ((b[0][0] === px && b[0][1] === py && b[1][0] === row && b[1][1] === col) ||
       (b[1][0] === px && b[1][1] === py && b[0][0] === row && b[0][1] === col)) &&
      !(b[0][0] === this.selectedBridgeToRotate[0][0] && b[0][1] === this.selectedBridgeToRotate[0][1] &&
        b[1][0] === this.selectedBridgeToRotate[1][0] && b[1][1] === this.selectedBridgeToRotate[1][1])
    );

    if (!foundInValidLocations) {
      console.log(`[DEBUG] ❌ Intersection (${row},${col}) REFUSÉE car non présente dans validBridgeLocations`);
      continue;
    }

    if (alreadyExists) {
      console.log(`[DEBUG] ❌ Intersection (${row},${col}) REFUSÉE car un pont existe déjà`);
      continue;
    }

    // Ajouter l'intersection valide
    console.log(`[DEBUG] ✅ Intersection ajoutée: (${row}, ${col})`);
    this.highlightIntersections.push({ row, col });
  }

  console.log("[DEBUG] ✅ Intersections valides finales:", this.highlightIntersections);
}
,


    // 2ᵉ clic => On finit la rotation
    finishBridgeRotation(row, col) {
    console.log(`[ROTATE] 2ᵉ clic intersection = (${row},${col})`);
    console.log("Intersections valides pour rotation:", this.highlightIntersections);

    const [px, py] = this.pivot;

    // Vérifier si la case cliquée est bien dans la liste des intersections valides
    const isInHighlight = this.highlightIntersections.some(h => h.row === row && h.col === col);
    if (!isInHighlight) {
        console.log("[DEBUG] ❌ Intersection non valide !");
        this.showFeedback("Cette intersection n’est pas valide pour la rotation du pont !", true);
        return;
    }

    // Vérifier si un pont existe déjà ici
    console.log("[DEBUG] Bridges AVANT rotation :", this.bridges);
    const alreadyExists = this.bridges.some(b =>
        (b[0][0] === px && b[0][1] === py && b[1][0] === row && b[1][1] === col) ||
        (b[1][0] === px && b[1][1] === py && b[0][0] === row && b[0][1] === col)
    );

    if (alreadyExists) {
        console.log("[DEBUG] ❌ Pont déjà existant !");
        this.showFeedback("Un pont existe déjà à cet emplacement !", true);
        return;
    }

    // Suppression de l'ancien pont
    console.log("[DEBUG] Suppression du pont existant:", this.selectedBridgeToRotate);
    this.removeBridge(this.selectedBridgeToRotate);

    // Ajout du nouveau pont
    console.log("[DEBUG] ✅ Ajout du nouveau pont:", [[px, py], [row, col]]);
    this.bridges.push([[px, py], [row, col]]);
    
    console.log("[DEBUG] 🔥 Bridges APRÈS rotation :", this.bridges);

    // Reset
    this.selectedBridgeToRotate = null;
    this.pivot = null;
    this.highlightIntersections = [];
    this.mustModifyBridge = false;
    this.nextPlayer();
}
,


    // Vérifie limites du plateau
    isValidInBoard(x,y){
      return x>=0 && x<6 && y>=0 && y<6;
    },

    // Tour suivant
    nextPlayer() {
      this.currentPlayerIndex = (this.currentPlayerIndex+1) % this.playersOrder.length;
      this.mode=null;
    }
  }
};
</script>

<style scoped>
.board-container {
  text-align: center;
  margin: 20px auto;
}

/* Feedback (erreur / succès) */
.feedback-banner {
  background: #4caf50; /* vert par défaut */
  color: white;
  font-weight: bold;
  text-align: center;
  padding: 10px;
  margin-top: 15px;
  border-radius: 5px;
}
.feedback-banner.error {
  background: #f44336; /* rouge */
}

/* Bannière d'avertissement */
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

/* Animation du lutin */
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

.lutin.animated {
  animation: moveLutin 0.3s ease-in-out;
}
</style>
