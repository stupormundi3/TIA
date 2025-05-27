<template>
  <!--
    Ce composant représente un seul "lutin", un petit cercle affiché
    à une position donnée (posX, posY) en fonction de sa ligne/colonne.
  -->
  <circle
    :cx="posX"
    :cy="posY"
    :r="lutinRadius "
    :fill="lutin.color"
    stroke="black"
    stroke-width="0.5"
    @click.stop="clicked"
  />
</template>

<script>
export default {
  name: "PontuLutin",
  props: {
    /*
      L'objet 'lutin' contient les informations :
        - color: la couleur du lutin
        - row  : la ligne sur la grille (0-indexée)
        - col  : la colonne sur la grille (0-indexée)
    */
    lutin: {
      type: Object,
      required: true
      // Exemple : { color: "green", row: 0, col: 2 }
    },
    // Rayon du cercle (correspond à votre prop lutinRadius ou circleRadius)
    lutinRadius: {
      type: Number,
      default: 10
    },
    // Espacement entre deux colonnes ou deux lignes (en pixels)
    spacing: {
      type: Number,
      default: 60
    }
  },
  computed: {
    /*
      posX/posY calculent la position réelle du lutin dans le <svg>.
      On prend sa colonne (ou ligne), qu'on multiplie par 'spacing'
      pour l'écart horizontal (ou vertical), puis on ajoute 'circleRadius'
      pour centrer parfaitement.
    */
    posX() {
      return this.lutin.col * this.spacing + this.lutinRadius ;
    },
    posY() {
      return this.lutin.row * this.spacing + this.lutinRadius ;
    }
  },
  methods: {
    /*
      "clicked" est appelée lorsqu'on clique sur le cercle.
      Elle émet un événement "clickedLutin" vers le parent
      en lui passant l'objet lutin.
    */
    clicked() {
      this.$emit("clickedLutin", this.lutin);
    }
  }
};
</script>

<style scoped>
/* Styles optionnels : par exemple changer l'opacité ou l’ombre */
circle {
  transition: none;
}
circle:hover {
  cursor: pointer;
}
</style>
