NAME = generated

LLC = llc
CC = clang

C_SRC = $(wildcard *.c)
LL_SRC = $(wildcard *.ll)

OBJ = $(C_SRC:.c=.o) $(LL_SRC:.ll=.o)

all: $(NAME)

$(NAME): $(OBJ)
	$(CC) $(CFLAGS) -o $(NAME) $(OBJ)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.ll
	$(LLC) -filetype=obj $<

clean: 
	rm -f $(OBJ)

fclean: clean

re: fclean all

.PHONY: all clean fclean re
