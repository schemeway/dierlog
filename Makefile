YAWS_INCLUDE=/usr/local/lib/yaws/include
GSERVER_INCLUDE=../nugramserver-clients/erlang

BEAM_DIR=ebin
SRC_DIR=src
INCLUDE_DIR=include

all: $(SRC_DIR)/*.erl
	@[ -d ebin ] || mkdir -p ebin
	erlc -v -I$(INCLUDE_DIR) -I$(GSERVER_INCLUDE) -I$(YAWS_INCLUDE) -o $(BEAM_DIR) $(SRC_DIR)/*.erl

clean:
	(cd ebin; rm -f *.beam)
	(cd log; rm -f *.log *.access)


tags: src/*.erl
	etags src/*.erl src/*.hrl


test: all
	erl -pa ebin

dialog-test: all
	erl -pa ebin -s dialog_test test

