PROJECT_NAME := glados
BUILD_DIR := dist-newstyle
EXECUTABLE := $(PROJECT_NAME)

BUILD_OUTPUT := $(shell cabal list-bin exe:glados)

COLOR_RESET := \033[0m
COLOR_BLUE := \033[1;34m
COLOR_GREEN := \033[1;32m

.PHONY: all
all: build

.PHONY: re
re: fclean all

.PHONY: build
build:
	cabal build
	@if [ -z "$(BUILD_OUTPUT)" ]; then \
		echo "Error: Binary not found. Ensure the project builds correctly."; \
		exit 1; \
	fi
	cp $(BUILD_OUTPUT) ./$(EXECUTABLE)

.PHONY: clean
clean:
	cabal clean

.PHONY: fclean
fclean: clean
	rm -rf $(BUILD_DIR) $(EXECUTABLE)

.PHONY: test
test:
	cabal test

.PHONY: repl
repl:
	cabal repl

.PHONY: format
format:
	ormolu -i $(shell find lib app test -name "*.hs")

.PHONY: help
help:
	@echo "$(COLOR_BLUE)Available targets:$(COLOR_RESET)"
	@echo "  $(COLOR_GREEN)all$(COLOR_RESET)      - Build the project"
	@echo "  $(COLOR_GREEN)re$(COLOR_RESET)       - Full clean and rebuild"
	@echo "  $(COLOR_GREEN)build$(COLOR_RESET)    - Build the project and copy the binary to the root level"
	@echo "  $(COLOR_GREEN)clean$(COLOR_RESET)    - Clean build artifacts"
	@echo "  $(COLOR_GREEN)fclean$(COLOR_RESET)   - Full clean, including dist directories"
	@echo "  $(COLOR_GREEN)test$(COLOR_RESET)     - Run tests"
	@echo "  $(COLOR_GREEN)repl$(COLOR_RESET)     - Launch interactive REPL"
	@echo "  $(COLOR_GREEN)format$(COLOR_RESET)   - Format all Haskell files using Fourmolu"
	@echo "  $(COLOR_GREEN)help$(COLOR_RESET)     - Display this help message"
