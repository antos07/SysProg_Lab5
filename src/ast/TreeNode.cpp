//
// Created by antos07 on 11/13/23.
//

#include <cassert>
#include <utility>
#include "TreeNode.hpp"

namespace ast {
TreeNode::TreeNode(std::string name) : name_{std::move(name)}, children_{} {

}

TreeNode::TreeNode(std::string name, std::vector<std::unique_ptr<TreeNode>> children) : name_{std::move(name)},
                                                                                        children_{std::move(children)} {
}

void TreeNode::print(std::ostream &out) const {
  print(out, 0);
}

void TreeNode::print(std::ostream &out, std::size_t indent_size) const {
  for (std::size_t i = 0; i < indent_size; ++i) {
    out << ' ';
  }
  out << name_ << ":\n";
  for (const auto &child : children_) {
    child->print(out, indent_size + 1);
  }
}

void TreeNode::append_child(std::unique_ptr<TreeNode> &&child) {
  children_.emplace_back(std::move(child));
}

std::vector<const TreeNode *> TreeNode::children() const {
  std::vector<const TreeNode *> children{};
  for (const auto &child : children_) {
    children.emplace_back(child.get());
  }
  return children;
}

const std::string &TreeNode::name() const {
  return name_;
}

std::unique_ptr<TreeNode> make_node(std::string name) {
  return std::make_unique<TreeNode>(std::move(name));
}
}
