DUNE=dune
SED=sed
TARGET_JS=./_build/default/bin/js/main.bc.js
OUTPUT_JS=./react-trace.bc.js

all: $(OUTPUT_JS)

$(TARGET_JS):
	$(DUNE) build --profile=prod

$(OUTPUT_JS): $(TARGET_JS)
	$(SED) -E 's/(^|[^[:alnum:]_])node:/\1/g' $(TARGET_JS) >$(OUTPUT_JS)

clean:
	rm -f $(OUTPUT_JS)

rebuild: clean all

.PHONY: all clean rebuild
