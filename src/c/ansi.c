#include ".\ansi.h"

HANDLE stdoutHandle;
DWORD outModeInit;

void ansi_enable() {
  DWORD outMode = 0;

  stdoutHandle = GetStdHandle(STD_OUTPUT_HANDLE);

  if(stdoutHandle==INVALID_HANDLE_VALUE) {
    exit(GetLastError());
  }

  if(!GetConsoleMode(stdoutHandle, &outMode)) {
    exit(GetLastError());
  }

  outModeInit = outMode;

  outMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;

  if(!SetConsoleMode(stdoutHandle, outMode)) {
    exit(GetLastError());
  }
}

void ansi_disable() {
  if(!SetConsoleMode(stdoutHandle, outModeInit)) {
    exit(GetLastError());
  }
}
