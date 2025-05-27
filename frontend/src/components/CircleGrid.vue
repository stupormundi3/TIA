<template>
  <div class="svg-container">
    <svg :width="width" :height="height">
      
      <!-- Ponts sous forme de composants enfant -->
      <PontuPont
      v-for="(bridge, index) in validBridges"
      :key="'pont-' + index"
      :pont="convertToPontObject(bridge)"
      :gridCoords="bridge"
      @pont-clicked="deleteBridgeByCoords"
      @pont-rotate="rotateBridgeByCoords"
    />


      <!-- Cercles (intersections) -->
      <circle
        v-for="(pt, idx) in points"
        :key="'pt-'+idx"
        :cx="pt.x"
        :cy="pt.y"
        :r="circleRadius"
        :fill="isHighlighted(pt) ? '#b3ffb3' : '#dddddd'"   
        stroke="#888"
        stroke-width="0.5"
        @click="intersectionClicked(pt.row, pt.col)"
      />

      <!-- Lutins -->
      <PontuLutin
        v-for="(lutin, idx) in lutins"
        :key="'lutin-'+idx"
        :lutin="lutin"
        :spacing="spacing"
        :circleRadius="circleRadius"
        @clickedLutin="onLutinClick"
      />

    </svg>

    <!-- Tooltip ou message contextuel (facultatif) -->
    <!-- On pourrait ajouter un tooltip dynamique pour un pont survolé, etc. -->
  </div>
</template>

<script>
import PontuLutin from "./Lutin.vue";
import PontuPont from "./Pont.vue";

export default {
  name: "CircleGrid",
  components: { PontuPont, PontuLutin },

  props: {
    rows: { type: Number, default: 6 },
    cols: { type: Number, default: 6 },
    spacing: { type: Number, default: 60 },
    circleRadius: { type: Number, default: 10 },
    lutinRadius: { type: Number, default: 8 },
    lutins: { type: Array, default: () => [] },
    bridges: { type: Array, default: () => [] },
    highlightIntersections: { type: Array, default: () => [] }
  },

  computed: {
    width() {
      return (this.cols - 1) * this.spacing + this.circleRadius * 2;
    },
    height() {
      return (this.rows - 1) * this.spacing + this.circleRadius * 2;
    },
    points() {
      const pts = [];
      for (let r = 0; r < this.rows; r++) {
        for (let c = 0; c < this.cols; c++) {
          pts.push({
            x: c * this.spacing + this.circleRadius,
            y: r * this.spacing + this.circleRadius,
            row: r,
            col: c
          });
        }
      }
      return pts;
    },
    validBridges() {
      // On ne garde que les ponts valides (convertToPontObject ne retourne pas null)
      return this.bridges.filter(b => this.convertToPontObject(b));
    }
  },

  methods: {
    // 🔁 Appelé lors du clic sur un pont (simple clic)
    async deleteBridgeByCoords(gridCoords) {
      try {
        // Suppression locale (ou appeler API si disponible)
        this.$emit("delete-bridge", gridCoords);
        this.$emit("refresh-game");
      } catch (e) {
        console.error("Erreur suppression pont :", e);
      }
    },

    // 🔁 Appelé lors du double clic
    async rotateBridgeByCoords(gridCoords) {
      try {
        this.$emit("rotate-bridge", gridCoords);
        this.$emit("refresh-game");
      } catch (e) {
        console.error("Erreur rotation pont :", e);
      }
    },

   convertToPontObject(bridge) {
  // Vérifier qu'on a bien deux paires de coordonnées [ [r1,c1], [r2,c2] ]
  if (
    !Array.isArray(bridge) ||
    bridge.length !== 2 ||
    !Array.isArray(bridge[0]) || bridge[0].length !== 2 ||
    !Array.isArray(bridge[1]) || bridge[1].length !== 2
  ) {
    console.warn('Pont mal formé, on l’ignore :', bridge);
    return null;
  }

  const [[r1, c1], [r2, c2]] = bridge;
  const idx1 = r1 * this.cols + c1;
  const idx2 = r2 * this.cols + c2;
  const p1 = this.points[idx1];
  const p2 = this.points[idx2];

  // Si hors grille, on ne tente pas de dessiner
  if (!p1 || !p2) {
    console.warn('Coordonnées hors grille, on ignore :', bridge);
    return null;
  }

  return { x1: p1.x, y1: p1.y, x2: p2.x, y2: p2.y };
},


    onLutinClick(lutin) {
      this.$emit("lutin-clicked", lutin);
    },

    intersectionClicked(row, col) {
      this.$emit("intersection-clicked", { x: col, y: row });
    },

    getRowColFromXY(x, y) {
      const colFloat = (x - this.circleRadius) / this.spacing;
      const rowFloat = (y - this.circleRadius) / this.spacing;
      return [Math.round(rowFloat), Math.round(colFloat)];
    },

    findBridgeInGrid({ x1, y1, x2, y2 }) {
      const r1c1 = this.getRowColFromXY(x1, y1);
      const r2c2 = this.getRowColFromXY(x2, y2);
      return [r1c1, r2c2];
    },

    isHighlighted(pt) {
      return this.highlightIntersections.some(
        h => h.row === pt.row && h.col === pt.col
      );
    }
  }
};
</script>

<style scoped>
.svg-container {
  position: relative;
}

svg {
  background: #fdfdfd;
}
</style>
