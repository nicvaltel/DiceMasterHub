// Create a WebSocket connection to your server
const socket = new WebSocket("ws://localhost:1234/ws");

// Get references to HTML elements
const lineContainer = document.getElementById('line-container');

// Event listener for when the WebSocket connection is opened
socket.addEventListener('open', (event) => {
    console.log('WebSocket connection opened:', event);
    // Send a welcome message or perform any initial setup here
});

// Event listener for incoming messages from the WebSocket server
socket.addEventListener('message', (event) => {
    const message = event.data;
    console.log('Received message:', message);
    updateLineContainer(message);
});

// Event listener for when the WebSocket connection is closed
socket.addEventListener('close', (event) => {
    console.log('WebSocket connection closed:', event);
    // You can handle reconnection or other actions here
});

// Function to update the line container with given lines
function updateLineContainer(message) {
    // Split the message by '\n' separator and update the lines
    const lines = message.split('\n');

    // Clear the existing lines
    lineContainer.innerHTML = '';

    // Add lines as div elements
    lines.forEach((line) => {
        const lineElement = document.createElement('div');
        lineElement.classList.add('line');
        lineElement.textContent = line;
        lineContainer.appendChild(lineElement);
    });
}