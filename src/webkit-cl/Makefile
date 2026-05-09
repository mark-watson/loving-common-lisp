# webkit-cl Makefile
# Builds libwebkit_cl.dylib from the Objective-C shim

CC = clang
CFLAGS = -fobjc-arc -fPIC -O2 -Wall
FRAMEWORKS = -framework Cocoa -framework WebKit
LDFLAGS = -dynamiclib -install_name @rpath/libwebkit_cl.dylib

TARGET = libwebkit_cl.dylib
SRC = webkit_cl.m

.PHONY: all clean debug

all: $(TARGET)

$(TARGET): $(SRC) webkit_cl.h
	$(CC) $(CFLAGS) $(FRAMEWORKS) $(LDFLAGS) -o $@ $<

debug: $(SRC) webkit_cl.h
	$(CC) $(CFLAGS) -DDEBUG $(FRAMEWORKS) $(LDFLAGS) -o $(TARGET) $<

clean:
	rm -f $(TARGET)
