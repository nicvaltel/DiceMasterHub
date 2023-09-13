// Create a WebSocket connection to your server
const socket = new WebSocket("ws://95.140.155.123:1234/ws"); // on remote server run # ./back-end-exe 95.140.155.123 1234

// Get references to HTML elements
const lobbyTableBody = document.getElementById('lobby-table-body');

// Event listener for when the WebSocket connection is opened
socket.addEventListener('open', (event) => {
    console.log('WebSocket connection opened:', event);
    // Send a welcome message or perform any initial setup here
});

// Event listener for incoming messages from the WebSocket server
socket.addEventListener('message', (event) => {
    const message = event.data;
    console.log('Received message:', message);
    updateLineTable(message);
});

// Event listener for when the WebSocket connection is closed
socket.addEventListener('close', (event) => {
    console.log('WebSocket connection closed:', event);
    // You can handle reconnection or other actions here
});



// Function to update the line table with given lines
function updateLineTable(message) {
    // Split the message by '\n' separator and update the lines
    const lines = message.split('\n');

    // Clear the existing table rows
    lobbyTableBody.innerHTML = '';

    // Add rows for each line
    lines.slice(0, 10).forEach((line, index) => {

        const rs = line.split(';');

        const row = document.createElement('tr');
        const rowNumCell = document.createElement('th');
        rowNumCell.setAttribute('scope', 'row');
        rowNumCell.textContent = (index + 1).toString();
        const messageCell = document.createElement('td');
        messageCell.textContent = rs[0];

        const firstCell = document.createElement('td');
        firstCell.textContent = rs[1];
        const lastCell = document.createElement('td');
        lastCell.textContent = rs[2];
        const handleCell = document.createElement('td');
        handleCell.textContent = rs[3];

        row.appendChild(rowNumCell);
        row.appendChild(messageCell);
        row.appendChild(firstCell);
        row.appendChild(lastCell);
        row.appendChild(handleCell);

        lobbyTableBody.appendChild(row);
    });
}