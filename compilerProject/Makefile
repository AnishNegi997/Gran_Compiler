CXX = g++
CC = gcc
CXXFLAGS = -std=c++17 -I./include $(shell llvm-config --cxxflags) -fexceptions
CFLAGS = -fPIC
LDFLAGS = $(shell llvm-config --ldflags --system-libs --libs core) -Wl,-rpath,'$$ORIGIN'

SRCS = src/main.cpp src/lexer.cpp src/parser.cpp src/ir_generator.cpp
OBJS = $(SRCS:.cpp=.o)
TARGET = gran
RUNTIME = libruntime.so

.PHONY: all clean

all: $(RUNTIME) $(TARGET)

$(RUNTIME): runtime.c
	$(CC) $(CFLAGS) -shared -o $@ $<

$(TARGET): $(OBJS)
	$(CXX) $(OBJS) -o $(TARGET) $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJS) $(TARGET) $(RUNTIME) 