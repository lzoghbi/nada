STACK        = stack
MAC_STACK    = `xcrun --show-sdk-path`/usr/include/ffi
DEFAULT_FILE = nada.org

.PHONY: all test build clean lint format

all: build format

build:
	$(STACK) build

mac:
	C_INCLUDE_PATH="$(MAC_STACK)" $(STACK) build

run:
	$(STACK) exec nada -- -f $(DEFAULT_FILE) edit

test:
	$(STACK) test

clean:
	$(STACK) clean

format:
	$(STACK) exec fourmolu -- -i -q src app

lint:
	$(STACK) exec fourmolu -- -m check -q src app
