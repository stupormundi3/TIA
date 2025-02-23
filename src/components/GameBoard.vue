<template>
  <div class="board-container">
    <h2>PontuXL - 6x6</h2>

    <CircleGrid
      :rows="6"
      :cols="6"
      :spacing="60"
      :circleRadius="10"
      :lutinRadius="8"
      :lutins="lutins"
      :bridges="bridges"
      :selectedLutin="selectedLutin"
      @lutin-clicked="onLutinClicked"
      @intersection-clicked="onIntersectionClicked"
      @delete-bridge="handleBridgeAction"
      @rotate-bridge="handleBridgeAction"
    />

    <!--  Message d'obligation de suppression ou rotation de pont -->
    <div v-if="mustModifyBridge" class="warning-banner">
      <p>⚠ Vous devez <b>supprimer</b> ou <b>tourner</b> un pont avant de terminer votre tour !</p>
      <button @click="mode = 'delete'" :class="{ active: mode === 'delete' }">Supprimer un pont</button>
      <button @click="mode = 'rotate'" :class="{ active: mode === 'rotate' }">Tourner un pont</button>
    </div>

    <h3>Tour actuel : {{ currentPlayer }}</h3>
    <h3>Ponts restants ({{ bridges.length }})</h3>
  </div>
</template>

<script>
import CircleGrid from "./CircleGrid.vue";

export default {
  name: "GameBoard",
  components: { CircleGrid },
  data() {
    return {
      lutins: [
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
        { color: "red", row: 5, col: 4 }
      ],

      bridges: [],
      playersOrder: ["green", "blue", "yellow", "red"],
      currentPlayerIndex: 0,
      selectedLutin: null,
      mustModifyBridge: false, // 🔥 Vérifie si le joueur doit modifier un pont
      mode: null // 🔥 "delete" pour supprimer, "rotate" pour tourner
    };
  },
  created() {
    this.bridges = this.generateInitialBridges();
  },
  computed: {
    currentPlayer() {
      return this.playersOrder[this.currentPlayerIndex];
    }
  },
  methods: {

    //  Animation fluide du déplacement des lutins
    animateMove(lutin, row, col) {
        if (!lutin) return; // Vérifier que le lutin existe
        lutin.animated = true;

        setTimeout(() => {
            lutin.row = row;
            lutin.col = col;
            lutin.animated = false; // ✅ Supprimer l’animation après le déplacement
        }, 300); // ⏳ Durée de l'animation (300ms)
    },
    generateInitialBridges() {
      let list = [];
      for (let row = 0; row < 6; row++) {
        for (let col = 0; col < 6; col++) {
          if (col < 5) list.push([[row, col], [row, col + 1]]);
          if (row < 5) list.push([[row, col], [row + 1, col]]);
        }
      }
      return list;
    },

    onLutinClicked(lutin) {
      if (lutin.color !== this.currentPlayer) {
        alert("Ce n’est pas le tour de " + lutin.color);
        return;
      }
      this.selectedLutin = lutin;
      this.mustModifyBridge = false;
    },

    onIntersectionClicked(row, col) {
    if (!this.selectedLutin) return;

    if (!this.isMoveValid(this.selectedLutin, row, col)) {
        alert("Déplacement invalide !");
        return;
    }

    // 🔥 Appliquer l'animation avant de changer les coordonnées
    this.animateMove(this.selectedLutin, row, col);

    this.mustModifyBridge = true; // Le joueur doit supprimer/tourner un pont après le déplacement
},


    isMoveValid(lutin, nr, nc) {
      if (nr < 0 || nr > 5 || nc < 0 || nc > 5) return false;
      const [r, c] = [lutin.row, lutin.col];

      if (!this.hasBridge(r, c, nr, nc)) {
        alert("Déplacement impossible : aucun pont !");
        return false;
      }

      const occupant = this.lutins.find(x => x.row === nr && x.col === nc);
      if (occupant) {
        alert("Déplacement impossible : case occupée !");
        return false;
      }

      return true;
    },

    hasBridge(r1, c1, r2, c2) {
      return this.bridges.some(
        b => 
          (b[0][0] === r1 && b[0][1] === c1 && b[1][0] === r2 && b[1][1] === c2) ||
          (b[1][0] === r1 && b[1][1] === c1 && b[0][0] === r2 && b[0][1] === c2)
      );
    },

    handleBridgeAction(bridge) {
      if (!this.mustModifyBridge) {
        alert("Vous devez d'abord déplacer un lutin !");
        return;
      }

      if (this.mode === "delete") {
        this.removeBridge(bridge);
      } else if (this.mode === "rotate") {
        this.rotateBridge(bridge);
      } else {
        alert("Veuillez sélectionner une action : supprimer ou tourner !");
      }
    },

    removeBridge(bridge) {
      this.bridges = this.bridges.filter(
        b => !(b[0][0] === bridge[0][0] && b[0][1] === bridge[0][1] &&
               b[1][0] === bridge[1][0] && b[1][1] === bridge[1][1])
      );

      this.mustModifyBridge = false;
      this.nextPlayer();
    },

    rotateBridge(bridge) {
        if (!this.mustModifyBridge) {
            alert("Vous devez d'abord déplacer un lutin !");
            return;
        }

        //  Si aucun pont n'est sélectionné, on sélectionne celui-ci pour rotation
        if (!this.selectedBridgeToRotate) {
            this.selectedBridgeToRotate = bridge;
            return; // Attendre que le joueur clique sur une nouvelle position
        }

        //  Étape 2 : Sélection de la nouvelle position
        const [[x1, y1], [x2, y2]] = this.selectedBridgeToRotate; // Ancien pont
        const [[nx1, ny1], [nx2, ny2]] = bridge; // Nouvel emplacement proposé

        //  Vérification : Le pont doit tourner sur la même intersection
        if (!this.sameIntersection([x1, y1], [nx1, ny1], [x2, y2], [nx2, ny2])) {
            this.selectedBridgeToRotate = null; // Réinitialiser la sélection
            return;
        }

        //  Vérification : Vérifier que la nouvelle position est libre
        const bridgeExists = this.bridges.some(b =>
            (b[0][0] === nx1 && b[0][1] === ny1 && b[1][0] === nx2 && b[1][1] === ny2) ||
            (b[0][0] === nx2 && b[0][1] === ny2 && b[1][0] === nx1 && b[1][1] === ny1)
        );

        if (bridgeExists) {
            this.selectedBridgeToRotate = null; // Réinitialiser la sélection
            return;
        }

        //  Étape 3 : Effectuer la rotation immédiatement
        this.removeBridge(this.selectedBridgeToRotate);
        this.bridges.push([[nx1, ny1], [nx2, ny2]]);
        
        this.mustModifyBridge = false; // ✅ Rotation validée
        this.selectedBridgeToRotate = null; // Réinitialiser la sélection
        this.nextPlayer();
    },

    

/*
  🛠️ Vérifie si une rotation est valide
  - Le nouveau pont doit être placé sur une intersection vide adjacente
*/
isValidRotation([x1, y1], [nx1, ny1]) {
    return (x1 === nx1 && y1 === ny1);
}
,

    /*
      Vérifie si deux intersections sont les mêmes
      (utile pour valider la rotation du pont)
    */
    sameIntersection([x1, y1], [x2, y2]) {
        return (x1 === x2 && y1 === y2);
    },


    nextPlayer() {
      this.currentPlayerIndex = (this.currentPlayerIndex + 1) % this.playersOrder.length;
      this.mode = null; // Réinitialise le mode après action
    }
  }
};
</script>

<style scoped>
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

.lutin.animated {
  animation: moveLutin 0.3s ease-in-out;
}
</style>
