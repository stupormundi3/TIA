<template>
  <div id="app">
    <!-- Barre de navigation ou autre -->
    

    <!-- utilisation de Gameboard directement -->
    <GameBoard />
  </div>
</template>

<script>
import GameBoard from "@/components/GameBoard.vue";

export default {
  name: "App",
  components: { GameBoard },
};
</script>

<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
}
</style>
