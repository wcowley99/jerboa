#include <format>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include "ast.h"

int main(int argc, char **argv) {
  // 5 + 5 + 10
  auto l1 = std::make_unique<LiteralExpr>(5);
  auto l2 = std::make_unique<LiteralExpr>(5);
  auto l3 = std::make_unique<LiteralExpr>(10);
  auto p1 = std::make_unique<BinaryExpr>(Op::ADD, std::move(l1), std::move(l2));
  auto p2 = std::make_unique<BinaryExpr>(Op::ADD, std::move(p1), std::move(l3));

  std::cout << p2->toAsm() << std::endl;

  std::ofstream file("out.s");

  if (file.is_open()) {
    file << std::format("section .text\n"
                        "global _start\n\n"
                        "_start:\n"
                        "  {}\n"
                        "  mov RAX, 60\n"
                        "  mov RDI, RCX\n"
                        "  syscall\n",
                        p2->toAsm());
    file.close();
  } else {
    std::cout << "Error opening file \"out.s\"" << std::endl;
  }

  system("nasm -f elf64 -o out.o out.s");
  system("ld -o b.out out.o");

  return 0;
}
