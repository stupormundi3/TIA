<template>
    <!--
      Ce composant gère une connexion WebSocket à un serveur (ws://localhost:8080).
      Il affiche simplement un bouton pour envoyer un message "Hello" au serveur.
    -->
    <div>
      <h3>WebSocket</h3>
      <!-- Bouton pour envoyer un message "Hello" via la websocket -->
      <button @click="sendHello">Envoyer 'Hello'</button>
    </div>
  </template>
  
  <script>
  export default {
    name: "WebSocket",
  
    data() {
      return {
        /*
          'socket' stocke l'objet WebSocket lui-même.
          Une fois initialisé (dans created), on peut l'utiliser
          pour envoyer/recevoir des messages.
        */
        socket: null
      };
    },
  
    /*
      Au moment où le composant est créé, on ouvre la connexion WebSocket
      vers "ws://localhost:8080".
      On définit également un gestionnaire d'événement pour 'onmessage'
      afin de réagir aux messages reçus du serveur.
    */
    created() {
      this.socket = new WebSocket("ws://localhost:8080");
  
      // Quand le serveur envoie un message, on appelle handleMessage()
      this.socket.onmessage = (ev) => this.handleMessage(ev.data);
    },
  
    methods: {
      /*
        handleMessage reçoit les données envoyées par le serveur.
        On les logue dans la console pour debug, ou on peut émettre
        un événement vers le parent (ex: this.$emit('websocketData', data)).
      */
      handleMessage(data) {
        console.log("Reçu:", data);
        // => this.$emit('websocketData', data) // Exemple si vous voulez transmettre la data au parent
      },
  
      /*
        sendHello envoie la chaîne de caractères "Hello from client"
        au serveur via la WebSocket.
      */
      sendHello() {
        this.socket.send("Hello from client");
      }
    }
  };
  </script>
  