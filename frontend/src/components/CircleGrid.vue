<template>
  <div class="svg-container">
    <svg :width="width" :height="height">
      <!-- Ponts sous forme de composants enfant -->
      <PontuPont
        v-for="(bridge, index) in validBridges"
        :key="'pont-' + index"
        :pont="convertToPontObject(bridge)"
        :gridCoords="bridge"
        @click.stop="onBridgeClick(bridge)"
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
        v-for="lutin in lutins"
        :key="lutin.color + '-' + lutin.row + '-' + lutin.col"
        :lutin="lutin"
        :spacing="spacing"
        :circleRadius="circleRadius"
        @clickedLutin="onLutinClick"
      />

      <!-- Ponts surlignés -->
      <line
        v-for="(line, index) in highlightLines"
        :key="'highlight-line-' + index"
        :x1="line.x1"
        :y1="line.y1"
        :x2="line.x2"
        :y2="line.y2"
        stroke="#ff0000"
        stroke-width="2"
        stroke-dasharray="4"
      />
    </svg>
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
    highlightBridges: { type: Array, default: () => [] },
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
      return this.bridges.filter(b => {
        if (!Array.isArray(b) || b.length !== 2) return false;
        const [[r1, c1], [r2, c2]] = b;
        const isHorizontal = r1 === r2 && Math.abs(c1 - c2) === 1;
        const isVertical = c1 === c2 && Math.abs(r1 - r2) === 1;
        return (isHorizontal || isVertical) && this.convertToPontObject(b);
      });
    },
    // Génère les tracés SVG pour les ponts à surligner
    highlightLines() {
      return this.highlightBridges
        .map(b => this.convertToPontObject(b))
        .filter(Boolean);
    }
  },

  methods: {
    onBridgeClick(gridCoords) {
      // émet vers GameBoard avec bridge + pivot
      const rawBridge = gridCoords.map(([r, c]) => [c + 1, r + 1]);
      const rawPivot = rawBridge[0];
      this.$emit("pont-action", { bridge: rawBridge, pivot: rawPivot });
    },

    convertToPontObject(bridge) {
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
    isHighlighted(pt) {
      return this.highlightIntersections.some(
        h => h.row === pt.row && h.col === pt.col
      );
    }
  }
};
</script>

<style scoped>
.svg-container { position: relative; }
svg { background: #fdfdfd; }
</style>
