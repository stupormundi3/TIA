<!-- eslint-disable vue/no-duplicate-attributes -->
<template>
  <div class="svg-container">
    <svg :width="width" :height="height">
      <!-- Ponts (lignes) -->
      <line
        v-for="(bridge, index) in bridges"
        :key="'bridge-' + index"
        :x1="points[bridge[0][0] * cols + bridge[0][1]].x"
        :y1="points[bridge[0][0] * cols + bridge[0][1]].y"
        :x2="points[bridge[1][0] * cols + bridge[1][1]].x"
        :y2="points[bridge[1][0] * cols + bridge[1][1]].y"
        @mouseover="hoverBridge(bridge, $event)"
        @mouseleave="unhoverBridge"
        @click="deleteBridge(bridge)"
        @dblclick="rotateBridge(bridge)"
        :stroke="isHovered(bridge) ? 'red' : 'black'"
        :stroke-width="isHovered(bridge) ? '6' : '4'"
      />

      <!-- Cercles (intersections) -->
      <circle
        v-for="(pt, idx) in points"
        :key="'pt-'+idx"
        :cx="pt.x"
        :cy="pt.y"
        :r="circleRadius"
        fill="#dddddd"
        stroke="#888"
        stroke-width="0.5"
        @click="intersectionClicked(pt.row, pt.col)"
      />

      <!-- Lutins -->
      <circle
        v-for="(lutin, idx) in lutins"
        :key="'lutin-'+idx"
        :cx="getLutinX(lutin)"
        :cy="getLutinY(lutin)"
        :r="lutinRadius"
        :fill="lutin.color"
        stroke="black"
        stroke-width="0.5"
        @click.stop="onLutinClick(lutin)"
      />
    </svg>

    <!-- Tooltip (info-bulle) pour afficher les coordonnées du pont -->
    <div v-if="hoveredBridge" class="tooltip" :style="{ top: tooltipY + 'px', left: tooltipX + 'px' }">
      Pont : ({{ hoveredBridge[0][0] }},{{ hoveredBridge[0][1] }}) - ({{ hoveredBridge[1][0] }},{{ hoveredBridge[1][1] }})
    </div>
  </div>
</template>

<script>
export default {
  name: "CircleGrid",
  props: {
    rows: { type: Number, default: 6 },
    cols: { type: Number, default: 6 },
    spacing: { type: Number, default: 60 },
    circleRadius: { type: Number, default: 10 },
    lutinRadius: { type: Number, default: 8 },
    lutins: { type: Array, default: () => [] },
    bridges: { type: Array, default: () => [] }
  },
  data() {
    return {
      hoveredBridge: null, // Stocke le pont survolé
      tooltipX: 0, // Position X du tooltip
      tooltipY: 0, // Position Y du tooltip
    };
  },
  computed: {
    width() {
      return (this.cols - 1) * this.spacing + this.circleRadius * 2;
    },
    height() {
      return (this.rows - 1) * this.spacing + this.circleRadius * 2;
    },

    /*
      Génère les intersections de la grille
    */
    points() {
      let pts = [];
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
    }
  },
  methods: {
    intersectionClicked(row, col) {
      this.$emit("intersection-clicked", row, col);
    },

    onLutinClick(lutin) {
      this.$emit("lutin-clicked", lutin);
    },

    getLutinX(lutin) {
      return lutin.col * this.spacing + this.circleRadius;
    },

    getLutinY(lutin) {
      return lutin.row * this.spacing + this.circleRadius;
    },

    /*
      Gestion du survol des ponts
    */
    hoverBridge(bridge, event) {
      this.hoveredBridge = bridge;
      this.tooltipX = event.pageX + 10; // Décalage pour éviter le curseur
      this.tooltipY = event.pageY - 30;
      this.$emit("bridge-hovered", bridge);
    },

    unhoverBridge() {
      this.hoveredBridge = null;
      this.$emit("bridge-hovered", null);
    },

    isHovered(bridge) {
      return this.hoveredBridge && 
        ((this.hoveredBridge[0][0] === bridge[0][0] && this.hoveredBridge[0][1] === bridge[0][1] &&
          this.hoveredBridge[1][0] === bridge[1][0] && this.hoveredBridge[1][1] === bridge[1][1]) ||
         (this.hoveredBridge[0][0] === bridge[1][0] && this.hoveredBridge[0][1] === bridge[1][1] &&
          this.hoveredBridge[1][0] === bridge[0][0] && this.hoveredBridge[1][1] === bridge[0][1]));
    },

    /*
      Suppression d’un pont au clic
    */
    deleteBridge(bridge) {
      this.$emit("delete-bridge", bridge);
    },

    /*
      Rotation d’un pont au double-clic
    */
    rotateBridge(bridge) {
      this.$emit("rotate-bridge", bridge);
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

.tooltip {
  position: absolute;
  background: black;
  color: white;
  padding: 5px;
  border-radius: 4px;
  font-size: 12px;
  pointer-events: none;
  white-space: nowrap;
}
</style>
