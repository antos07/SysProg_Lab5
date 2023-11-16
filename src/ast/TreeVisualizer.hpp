//
// Created by antos07 on 11/15/23.
//

#ifndef SYSPROG_LAB5_SRC_AST_TREEVISUALIZER_HPP_
#define SYSPROG_LAB5_SRC_AST_TREEVISUALIZER_HPP_

#include <filesystem>
#include <graphviz/cgraph.h>
#include <graphviz/gvc.h>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include "ast/TreeNode.hpp"

namespace ast {

class TreeVisualizer {
 public:
  void to_png(const TreeNode *tree_root, const std::filesystem::path &path);

  TreeVisualizer();
  ~TreeVisualizer();


 private:
  GVC_t *gvc_;
  std::size_t current_id_;
  std::vector<std::string> strings_;
  std::unordered_map<std::string_view, std::size_t> string_index_;

  char *unique_name();
  Agnode_s *fill_graph(const TreeNode *node, Agraph_s *graph);

  char *c_str(std::string string);
};

} // ast

#endif //SYSPROG_LAB5_SRC_AST_TREEVISUALIZER_HPP_
