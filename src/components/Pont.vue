<template>
    <!--
      Ce composant représente un "pont" entre deux points (x1,y1) et (x2,y2)
      dans un <svg>.
      On l'affiche en tant que ligne <line>.
    -->
    <line
      :x1="pont.x1"
      :y1="pont.y1"
      :x2="pont.x2"
      :y2="pont.y2"
      stroke="black"
      stroke-width="1"
      @click="onPontClick"
    />
  </template>
  
  <script>
  export default {
    name: "PontuPont",
    props: {
      /*
        La prop 'pont' est un objet contenant les coordonnées de la ligne :
         - x1, y1 : coordonnées de départ
         - x2, y2 : coordonnées d'arrivée
        Par exemple: { x1: 100, y1: 100, x2: 160, y2: 100 }
      */
      pont: {
        type: Object,
        required: true
      }
    },
    methods: {
      /*
        Quand on clique sur la ligne, on émet un événement "pontClicked"
        vers le composant parent, en lui passant l'objet 'pont'.
        Cela permet au parent de gérer l'action, par exemple pour
        supprimer ou "tourner" le pont, etc.
      */
      onPontClick() {
        this.$emit("pontClicked", this.pont);
      }
    }
  };
  </script>
  