#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include "../socket_helper.h"

#include <stdio.h>
#include <string.h>
#include <time.h>

int main() {
    printf("Configuring local address...\n");
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET6;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = AI_PASSIVE;

    struct addrinfo *bind_address;
    getaddrinfo(0, "8080", &hints, &bind_address);

    printf("Creating socket...\n");
    int socket_listen;
    socket_listen = socket(bind_address->ai_family,
                           bind_address->ai_socktype,
                           bind_address->ai_protocol);

    if (!(socket_listen >= 0)) {
        fprintf(stderr, "socket() failed. (%d)\n", errno);
        return 1;
    }

    /* Allow dual-stack socket to accept IPv4 and IPv6 requests */
    int option = 0;
    if (setsockopt(socket_listen, IPPROTO_IPV6, IPV6_V6ONLY, (void*)&option, sizeof(option))) {
        fprintf(stderr, "setsockopt() failed. (%s)\n", strerror(errno));
        return 1;
    }

    printf("Binding socket to local address...\n");
    /* bind() returns 0 on success */
    if (bind(socket_listen, bind_address->ai_addr, bind_address->ai_addrlen)) {
        fprintf(stderr, "bind() failed. (%d)\n", errno);
        return 1;
    }

    freeaddrinfo(bind_address);

    printf("Listening...\n");
    if (listen(socket_listen, 10) < 0) {
        fprintf(stderr, "listen() failed: (%d)\n", errno);
        return 1;
    }

    printf("Wainting for connection...\n");
    struct sockaddr_storage client_address;
    socklen_t client_len = sizeof(client_address);
    /* NOTE
       accept will:
       - block the program until a new connection is made
       - create a new socket when a connection is made, the original listen socket still listens for new connections,
           but the socket created by accept is used to send and receive data over the new connection
       - accept() fills in address info of connected client
    */
    int socket_client = accept(socket_listen, (struct sockaddr*) &client_address, &client_len);
    if (!(socket_client >= 0)) {
        fprintf(stderr, "accept() failed. (%d)\n", errno);
        return 1;
    }

    printf("Client is connected...");
    char address_buffer[100];
    getnameinfo((struct sockaddr*) &client_address, client_len, address_buffer, sizeof(address_buffer), 0, 0, NI_NUMERICHOST);
    printf("%s\n", address_buffer);

    printf("Reading request...\n");
    char request[1024];
    /* Ignoring here, but you sould always check that recv() > 0 in production */
    int bytes_received = recv(socket_client, request, 1024, 0);
    printf("Received %d bytes.\n", bytes_received);

    /* print request */
    /*
      can use the '*' character to specify the width precision, which in this case comes from 'bytes_received'.
       so this print says "print 'bytes_received' number of characters from the 'request' buffer
    */
    printf("%.*s", bytes_received, request);

    printf("Sending response...\n");
    const char *response =
        "HTTP/1.1 200 OK\r\n"
        "Connection: close\r\n"
        "Content-Type: text/plain\r\n"
        "\r\n"
        "Local time is: ";
    /* generally you should check that 'bytes_sent' was the number expected, and attempt to send the rest if not */
    int bytes_sent = send(socket_client, response, strlen(response), 0);
    printf("Sent %d of %d bytes.\n", bytes_sent, (int)strlen(response));

    time_t timer;
    time(&timer);
    char *time_msg = ctime(&timer);
    bytes_sent = send(socket_client, time_msg, strlen(time_msg), 0);
    printf("Sent %d of %d bytes.\n", bytes_sent, (int)strlen(time_msg));

    /* Close connection */
    printf("Closing connection...\n");
    close(socket_client);

    /* Close listening socket */
    printf("Closing listening socket...\n");
    close(socket_client);

    printf("Finished.\n");

    return 0;
}
