#include "ast.h"

#include <iostream>
#include <unordered_map>

std::string LiteralExpr::toString() { return std::format("{}", value); }

std::string LiteralExpr::toAsm() { return std::format("mov RCX, {}", value); }

std::string BinaryExpr::toString() {
  std::unordered_map<Op, std::string> ops = {
      {Op::ADD, "+"},
      {Op::SUB, "-"},
      {Op::DIV, "/"},
      {Op::MUL, "*"},
  };

  return std::format("{} {} {}", this->left->toString(), ops[this->op],
                     this->right->toString());
}

std::string BinaryExpr::toAsm() {
  switch (this->op) {
  case Op::ADD:
    return std::format("{}\n"
                       "mov RDX, RCX\n"
                       "{}\n"
                       "add RCX, RDX",
                       left->toAsm(), right->toAsm());
    break;
  default:
    std::cout << "Operator has not been defined yet!" << std::endl;
    break;
  }

  return "";
}
