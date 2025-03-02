<template>
  <div class="svg-container">
    <svg :width="width" :height="height">
      
      <!-- Ponts sous forme de composants enfant -->
      <PontuPont
        v-for="(bridge, index) in bridges"
        :key="'pont-' + index"
        :pont="convertToPontObject(bridge)"
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

    <!-- Tooltip ou message contextuel (facultatif) -->
    <!-- On pourrait ajouter un tooltip dynamique pour un pont survolé, etc. -->
  </div>
</template>

<script>
import PontuPont from "./Pont.vue"; // Composant enfant pour l'affichage d'un pont

export default {
  name: "CircleGrid",
  components: { PontuPont },

  props: {
    rows: { type: Number, default: 6 },
    cols: { type: Number, default: 6 },
    spacing: { type: Number, default: 60 },
    circleRadius: { type: Number, default: 10 },
    lutinRadius: { type: Number, default: 8 },

    /*
      Liste de lutins (ex: [{ color:'blue', row:2, col:3 }, ...])
    */
    lutins: { type: Array, default: () => [] },

    /*
      'bridges' est au format [ [ [r1,c1],[r2,c2] ], ... ]  
      c.-à-d. un tableau de ponts, 
      chaque pont étant un couple de coordonnées de grille.
    */
    bridges: { type: Array, default: () => [] },

    /*
      Optionnel: liste des intersections à mettre en surbrillance
      ex: [ { row:2, col:3 }, { row:2, col:4 }, ... ]
      Si tu n'en as pas besoin, tu peux enlever la logique isHighlighted()
    */
    highlightIntersections: {
      type: Array,
      default: () => []
    }
  },

  computed: {
    // Largeur totale du SVG
    width() {
      return (this.cols - 1) * this.spacing + this.circleRadius * 2;
    },
    // Hauteur totale du SVG
    height() {
      return (this.rows - 1) * this.spacing + this.circleRadius * 2;
    },

    /*
      Génère la liste des intersections de la grille => un tableau
      où chaque élément est { x, y, row, col }
    */
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
    }
  },

  methods: {
    /* =========================================================================
       Convertit un pont en format [ [r1,c1],[r2,c2] ]
       en un objet { x1, y1, x2, y2 } basé sur 'points' (coordonnées en px).
       ========================================================================= */
    convertToPontObject(bridge) {
      // bridge = [ [r1,c1], [r2,c2] ]
      const [[r1, c1], [r2, c2]] = bridge;

      // Récupère la coordonnée en pixels pour (r1,c1)
      const p1 = this.points[r1 * this.cols + c1];
      // idem pour (r2,c2)
      const p2 = this.points[r2 * this.cols + c2];

      return {
        x1: p1.x,
        y1: p1.y,
        x2: p2.x,
        y2: p2.y
      };
    },

    /* =============================
       GESTION DES LUTINS
       ============================= */
    intersectionClicked(row, col) {
      // Émet un événement vers le parent (GameBoard) 
      // pour gérer un éventuel déplacement ou rotation
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

    /* =============================
       GESTION DES PONTS
       ============================= */

    // Clic simple => suppression => On émet "delete-bridge" avec la forme [ [r1,c1],[r2,c2] ]
    deleteBridgeByCoords(pontObj) {
      // pontObj = { x1,y1,x2,y2 }
      // On retrouve dans 'bridges' le pont [ [r1,c1],[r2,c2] ]
      const rawBridge = this.findBridgeInGrid(pontObj);
      this.$emit("delete-bridge", rawBridge);
    },

    // Double-clic => rotation => on émet "rotate-bridge"
    rotateBridgeByCoords(pontObj) {
      const rawBridge = this.findBridgeInGrid(pontObj);
      this.$emit("rotate-bridge", rawBridge);
    },

    /* =============================
       Convertir coords (x1,y1) en (row,col)
    ============================= */
    findBridgeInGrid({ x1, y1, x2, y2 }) {
      const r1c1 = this.getRowColFromXY(x1, y1);
      const r2c2 = this.getRowColFromXY(x2, y2);
      return [r1c1, r2c2]; // => [ [r1,c1],[r2,c2] ]
    },

    getRowColFromXY(x, y) {
      // On inverse la logique de points() 
      // col ≈ (x - circleRadius)/spacing
      // row ≈ (y - circleRadius)/spacing
      const colFloat = (x - this.circleRadius) / this.spacing;
      const rowFloat = (y - this.circleRadius) / this.spacing;

      // On arrondit
      const rr = Math.round(rowFloat);
      const cc = Math.round(colFloat);
      return [rr, cc];
    },

    /* ============================
       SURBRILLANCE DES INTERSECTIONS
       ============================ */
    isHighlighted(pt) {
      // Vérifie si (pt.row, pt.col) est dans highlightIntersections
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
