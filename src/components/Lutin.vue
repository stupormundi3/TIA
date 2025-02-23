<template>
    <!--
      Ce composant représente un seul "lutin", un petit cercle affiché
      à une position donnée (posX, posY) en fonction de sa ligne/colonne.
    -->
    <circle
      :cx="posX"
      :cy="posY"
      :r="8"
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
        L' objet 'lutin' contient les informations :
          - color: la couleur du lutin
          - row  : la ligne sur la grille
          - col  : la colonne sur la grille
      */
      lutin: {
        type: Object,
        required: true // On exige la présence de la prop 'lutin'
        // Par exemple : { color: "green", row: 0, col: 2 }
      },
      // Rayon du cercle de base qui décale la grille
      circleRadius: { type: Number, default: 10 },
      // Espacement entre deux colonnes ou deux lignes
      spacing: { type: Number, default: 60 }
    },
    computed: {
      /*
        posX/posY calculent la position réelle du lutin dans le <svg>.
        On prend sa colonne (ou ligne), qu'on multiplie par 'spacing'
        pour l'écart horizontal (ou vertical), puis on ajoute 'circleRadius'
        pour aligner correctement sur le centre de la grille.
      */
      posX() {
        return this.lutin.col * this.spacing + this.circleRadius;
      },
      posY() {
        return this.lutin.row * this.spacing + this.circleRadius;
      }
    },
    methods: {
      /*
        "clicked" est appelée lorsqu'on clique sur le cercle.
        Elle émet un événement "clickedLutin" vers le parent,
        en lui transmettant l'objet lutin (this.lutin).
      */
      clicked() {
        this.$emit("clickedLutin", this.lutin);
      }
    }
  };
  </script>
  
  <style scoped>
  /* 
    Vous pouvez ajouter ici des styles spécifiques si nécessaire,
    par exemple changer la couleur de contour, l'opacité, etc.
  */
  </style>
  