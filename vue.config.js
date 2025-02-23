const { defineConfig } = require('@vue/cli-service');
const path = require("path"); // Ajoute cette ligne pour éviter l'erreur "path is not defined"

module.exports = defineConfig({
  transpileDependencies: true,
  configureWebpack: {
    resolve: {
      alias: {
        "@": path.resolve(__dirname, "src"), // Définit "@" comme alias pour "src"
      },
    },
  },
});
