#pragma once

#include <format>
#include <memory>
#include <string>

struct Expr {
  virtual ~Expr() = default;

  virtual std::string toString() = 0;
  virtual std::string toAsm() = 0;
};

struct LiteralExpr : public Expr {
  int value;

  explicit LiteralExpr(int val) : value(val) {}

  std::string toString() override;
  std::string toAsm() override;
};

typedef enum { ADD, SUB, DIV, MUL } Op;
struct BinaryExpr : public Expr {
  Op op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;

  BinaryExpr(Op op, std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs)
      : op(op), left(std::move(lhs)), right(std::move(rhs)) {}

  std::string toString() override;
  std::string toAsm() override;
};
