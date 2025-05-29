<template>
  <circle
    :cx="posX"
    :cy="posY"
    :r="lutinRadius"
    :fill="lutin.color"
    stroke="black"
    stroke-width="0.5"
    @click="handleClick($event)"
  />
</template>

<script>
export default {
  name: "PontuLutin",
  props: {
    lutin: { type: Object, required: true },
    lutinRadius: { type: Number, default: 10 },
    spacing: { type: Number, default: 60 }
  },
  computed: {
    posX() {
      return this.lutin.col * this.spacing + this.lutinRadius;
    },
    posY() {
      return this.lutin.row * this.spacing + this.lutinRadius;
    }
  },
 methods: {
    handleClick(evt) {
      console.log("[Lutin.vue] CLIC SUR LUTIN, event target:", evt.target);
      evt.stopPropagation();
      console.log("[Lutin.vue] Après stopPropagation, émet clickedLutin");
      this.$emit("clickedLutin", this.lutin);
    }
  }
};
</script>
