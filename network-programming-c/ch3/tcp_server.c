/* tcp_server.c */

#include "../socket_headers.h"
#include <ctype.h>

int main() {
    printf("Configuring local address...");
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    struct addrinfo *bind_address;
    /* specifying NULL or 0 for the hostname will tell getaddrinfo to set the address to any */
    getaddrinfo(0, "8080", &hints, &bind_address);

    printf("Creating socket...\n");
    int socket_listen;
    socket_listen = socket(bind_address->ai_family, bind_address->ai_socktype, bind_address->ai_protocol);
    if (socket_listen < 0) {
        fprintf(stderr, "socket() failed. (%d)\n", errno);
        return 1;
    }

    printf("Binding socket to local address...\n");
    if (bind(socket_listen, bind_address->ai_addr, bind_address->ai_addrlen)) {
        fprintf(stderr, "bind() failed. (%d)\n", errno);
        return 1;
    }
    /* Now that the socket is bound we can free the addrinfo struct */
    freeaddrinfo(bind_address);

    printf("Listening...\n");
    if (listen(socket_listen, 10) < 0) {
        fprintf(stderr, "listen() failed. (%d)\n", errno);
        return 1;
    }

    /* Define fd_set struct to store all active sockets, and track the max_socket to pass to select */
    fd_set master;
    FD_ZERO(&master);
    FD_SET(socket_listen, &master);
    int max_socket = socket_listen;

    printf("Waiting for connections...\n");

    while(1) {
        fd_set reads;
        /* copy master into reads because select() will modify the fd_set */
        reads = master;
        if (select(max_socket+1, &reads, 0, 0, 0) < 0) {
            fprintf(stderr, "select() failed: (%d)\n", errno);
            return 1;
        }

        int i;
        for(i = 1; i <= max_socket; ++i) {
            if (FD_ISSET(i, &reads)) {
                if (i == socket_listen) {
                    struct sockaddr_storage client_address;
                    socklen_t client_len = sizeof(client_address);
                    int socket_client = accept(socket_listen, (struct sockaddr*) &client_address, &client_len);
                    if (socket_client < 0) {
                        fprintf(stderr, "accept() failed. (%d)\n", errno);
                        return 1;
                    }

                    FD_SET(socket_client, &master);
                    if (socket_client > max_socket) {
                        max_socket = socket_client;
                    }

                    char address_buffer[100];
                    getnameinfo((struct sockaddr*)&client_address, client_len, address_buffer, sizeof(address_buffer), 0, 0, NI_NUMERICHOST);
                    printf("New connection from %s\n", address_buffer);
                } else {
                    char read[1024];
                    int bytes_received = recv(i, read, 1024, 0);
                    if (bytes_received < 1) {
                        /* remove socket from master fd_set, client closed the connection */
                        FD_CLR(i, &master);
                        close(i);
                        continue;
                    }
                    int j;
                    for (j = 0; j < bytes_received; ++j) {
                        read[j] = toupper(read[j]);
                    }
                    send(i, read, bytes_received, 0);
                }
            }
        }
    }

    printf("Closing listening socket...\n");
    close(socket_listen);

    printf("Finished.\n");
    return 0;
}
