NAME = llvm-ir-poc

GREEN = \033[0;32m
RED = \033[0;31m
YELLOW = \033[1;33m
MAGENTA = \033[0;35m
ORANGE = \033[0;33m
CYAN = \033[0;36m
BLUE = \033[0;34m
NC = \033[0m

ALL: $(NAME)

SRC = $(wildcard src/*.ll)
GLUE = $(wildcard src/*.c)

OBJ = $(SRC:.ll=.o) $(GLUE:.c=.o)

$(NAME): $(OBJ)
	@echo ">> $(MAGENTA)$@$(NC)"
	@$(CC) $(CFLAGS) -o $@ $^

%.o: %.ll
	@echo ">> $(YELLOW)$<$(CYAN) -> $(BLUE)$@$(NC)"
	@$(CC) $(CFLAGS) -c -o $@ $<

%.o: %.c
	@echo ">> $(ORANGE)$<$(CYAN) -> $(GREEN)$@$(NC)"
	@$(CC) $(CFLAGS) -c -o $@ $<

clean:
	@echo ">> $(RED)clean$(NC)"
	@rm -f $(NAME) $(OBJ)

re: clean ALL

.PHONY: ALL clean
