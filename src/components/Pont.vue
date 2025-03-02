<template>
  <!--
    Composant enfant qui représente un pont (une simple ligne <line>) 
    dans le SVG. 
  -->
  <line
    :x1="pont.x1"
    :y1="pont.y1"
    :x2="pont.x2"
    :y2="pont.y2"

    :stroke="isHovered ? 'red' : 'black'"
    :stroke-width="isHovered ? 3 : 1"

    @mouseover="hover(true)"
    @mouseleave="hover(false)"
    @click="onPontClick"
    @dblclick="onPontDoubleClick"
  />
</template>

<script>
export default {
  name: "PontuPont",
  props: {
    /*
      'pont' est un objet { x1, y1, x2, y2 } représentant
      les coordonnées (en pixels) de la ligne <line>.
    */
    pont: {
      type: Object,
      required: true
    }
  },
  data() {
    return {
      isHovered: false // indique si le pont est survolé pour changer la couleur/épaisseur
    };
  },
  methods: {
    // Survol du pont
    hover(val) {
      this.isHovered = val;
    },

    // Clic simple => émet un événement 'pont-clicked' vers le parent
    onPontClick() {
      this.$emit("pont-clicked", this.pont);
    },

    // Double-clic => émet un événement 'pont-rotate' pour rotation
    onPontDoubleClick() {
      this.$emit("pont-rotate", this.pont);
    }
  }
};
</script>

<style scoped>
/* 
  Pas de style additionnel, on gère l'apparence (couleur, épaisseur)
  via :stroke et :stroke-width dynamiques (isHovered).
*/
</style>
