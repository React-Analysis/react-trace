DUNE=dune
ESBUILD=bunx esbuild
TARGET_JS=./_build/default/bin/js/main.bc.js
OUTPUT_JS=./react-trace.bc.js

ESBUILD_FLAGS=--outfile=$(OUTPUT_JS) --bundle --platform=browser \
              --external:node:child_process --external:node:tty \
              --external:node:constants --minify --target=es2020

all: $(OUTPUT_JS)

$(TARGET_JS):
	$(DUNE) build --profile=prod

$(OUTPUT_JS): $(TARGET_JS)
	$(ESBUILD) $(TARGET_JS) $(ESBUILD_FLAGS)

clean:
	$(DUNE) clean
	rm -f $(OUTPUT_JS)

rebuild: clean all

.PHONY: all clean rebuild
