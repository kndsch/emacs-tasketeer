# Example Makefile for testing tasketeer functionality
# Demonstrates various target types and dependencies

# Variables
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
SRCDIR = src
BUILDDIR = build
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)
TARGET = $(BUILDDIR)/myapp

# Default target
all: $(TARGET)

# Build the main application
$(TARGET): $(OBJECTS) | $(BUILDDIR)
	$(CC) $(OBJECTS) -o $@

# Compile source files
$(BUILDDIR)/%.o: $(SRCDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Create build directory
$(BUILDDIR):
	mkdir -p $(BUILDDIR)

# Development targets
build: $(TARGET)

compile: $(OBJECTS)

test: $(TARGET)
	@echo "Running tests..."
	./$(TARGET) --test

# Utility targets
clean:
	rm -rf $(BUILDDIR)

install: $(TARGET)
	@echo "Installing application..."
	cp $(TARGET) /usr/local/bin/

uninstall:
	rm -f /usr/local/bin/myapp

# Development workflow
dev: clean build test

# Release workflow
release: clean build test package

package: $(TARGET)
	@echo "Creating package..."
	tar -czf myapp-release.tar.gz $(TARGET) README.md

# Linting and formatting
lint:
	@echo "Running linter..."
	cppcheck $(SRCDIR)/*.c

format:
	@echo "Formatting code..."
	clang-format -i $(SRCDIR)/*.c

# Documentation
docs:
	@echo "Generating documentation..."
	doxygen Doxyfile

# Docker targets
docker-build:
	docker build -t myapp .

docker-run: docker-build
	docker run --rm myapp

# Benchmarking
benchmark: $(TARGET)
	@echo "Running benchmarks..."
	./$(TARGET) --benchmark

# Debug build
debug: CFLAGS += -g -DDEBUG
debug: $(TARGET)

# Help target
help:
	@echo "Available targets:"
	@echo "  all       - Build the application (default)"
	@echo "  build     - Build the application"
	@echo "  compile   - Compile source files"
	@echo "  test      - Run tests"
	@echo "  clean     - Remove build files"
	@echo "  install   - Install application"
	@echo "  dev       - Development workflow"
	@echo "  release   - Release workflow"
	@echo "  lint      - Run linter"
	@echo "  format    - Format code"
	@echo "  docs      - Generate documentation"
	@echo "  docker-*  - Docker operations"
	@echo "  debug     - Build with debug symbols"
	@echo "  help      - Show this help"

# Phony targets (don't correspond to files)
.PHONY: all build compile test clean install uninstall dev release package lint format docs docker-build docker-run benchmark debug help