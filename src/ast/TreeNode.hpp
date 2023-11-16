//
// Created by antos07 on 11/13/23.
//

#ifndef SYSPROG_LAB5_SRC_AST_TREENODE_HPP_
#define SYSPROG_LAB5_SRC_AST_TREENODE_HPP_

#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <initializer_list>

namespace ast {

class TreeNode {
  std::string name_;
  std::vector<std::unique_ptr<TreeNode>> children_;

 public:
  explicit TreeNode(std::string name);

  TreeNode(std::string name, std::vector<std::unique_ptr<TreeNode>> children);

  void append_child(std::unique_ptr<TreeNode> &&child);

  [[nodiscard]] std::vector<const TreeNode *> children() const;

  const std::string &name() const;

  void print(std::ostream &out) const;

 private:
  void print(std::ostream &out, std::size_t indent_size) const;
};

std::unique_ptr<TreeNode> make_node(std::string name);
}

#endif //SYSPROG_LAB5_SRC_AST_TREENODE_HPP_
