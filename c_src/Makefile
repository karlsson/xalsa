# Based on Makefile from https://github.com/ConnorRigby/hello_calendar

ifeq ($(ERL_EI_INCLUDE_DIR),)
ERL_ROOT_DIR = $(shell erl -eval "io:format(\"~s~n\", [code:root_dir()])" -s init stop -noshell)
ifeq ($(ERL_ROOT_DIR),)
   $(error Could not find the Erlang installation. Check to see that 'erl' is in your PATH)
endif
ERL_EI_INCLUDE_DIR = $(ERL_ROOT_DIR)/usr/include
ERL_EI_LIBDIR = $(ERL_ROOT_DIR)/usr/lib
endif

# Set Erlang-specific compile and linker flags
ERL_CFLAGS ?= -I$(ERL_EI_INCLUDE_DIR)
ERL_LDLIBS ?= -L$(ERL_EI_LIBDIR)
LDLIBS += -lasound

CFLAGS ?= -fpic -std=c99 -O3 -Wall -Wextra -finline-functions
CFLAGS += -Wmissing-prototypes -Wno-unused-parameter -Wno-missing-field-initializers
LDFLAGS += -shared

ifeq ($(CROSSCOMPILE),)
ifeq ($(shell uname),Darwin)
LDFLAGS += -undefined dynamic_lookup
endif
ifeq ($(shell uname -m),x86_64)
CFLAGS += -mavx
endif
endif

NIF=../priv/xalsa.so

all: ../priv $(NIF)

../priv:
	@mkdir -p ../priv

$(NIF): xalsa_nif.c
	$(CC) $(ERL_CFLAGS) $(CFLAGS) $< $(LDFLAGS) $(ERL_LDLIBS) $(LDLIBS) -o $@

clean:
	@rm -f $(NIF)
