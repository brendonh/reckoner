PKG_NAME = rkgate

DEP_INCLUDES = -I ../deps/bson/include -I ../deps/mongodb/include
DEP_EBINS = -pa ../deps/bson/ebin -pa ../deps/mongodb/ebin

SRC_DIR = src
EBIN_DIR = ebin
INCLUDE_DIR = include

SOURCES  = $(wildcard $(SRC_DIR)/*.erl)
INCLUDES = $(wildcard $(INCLUDE_DIR)/*.hrl)
PARSERS = $(wildcart $(PRIV_DIR)/*.yrl)

TARGETS  = $(patsubst $(SRC_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES))

ERLC_INCLUDES = -I $(INCLUDE_DIR) $(DEP_INCLUDES)
ERL_EBINS = -pa $(EBIN_DIR) $(DEP_EBINS)

ERLC = erlc
ERLC_OPTS = $(ERLC_INCLUDES) -o $(EBIN_DIR) -Wall +debug_info

ERL_CMD=erl \
	-boot start_sasl \
	-config $(PKG_NAME) \
	+W w \
	$(ERL_EBINS)

all: $(TARGETS)

run_prereqs: all

run: run_prereqs
	$(ERL_CMD) -s rkgate_app launch

stop:
	erl_call -a '$(PKG_NAME)_app stop_and_halt []' -sname $(PKG_NAME)

clean: cleanlog
	rm -f $(EBIN_DIR)/*.beam

cleanlog:
	rm -f *.log *.access

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl $(INCLUDES)
	$(ERLC) $(ERLC_OPTS) $<

