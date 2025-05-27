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
  
  devServer: {
    proxy: {
      '/api': {
        target: 'http://localhost:8000', // port du backend Prolog
        changeOrigin: true,
        pathRewrite: { '^/api': '/api' } // Optionnel selon le backend
      }
    }
  }

});
