STACK       = stack

.PHONY: all test build clean lint format

all: build format

build:
	$(STACK) build

test:
	$(STACK) test

clean:
	$(STACK) clean

format:
	$(STACK) exec fourmolu -- -i -q src app

lint:
	$(STACK) exec fourmolu -- -m check -q src app
