//
// Created by antos07 on 11/15/23.
//

#include "TreeVisualizer.hpp"

namespace ast {
void TreeVisualizer::to_png(const TreeNode *node, const std::filesystem::path &path) {
  Agraph_t *g = agopen(c_str("g"), Agstrictdirected, nullptr);
  Agnode_s *gv_root = fill_graph(node, g);
  agsafeset(gv_root, c_str("color"), c_str("green"), c_str(""));
  agsafeset(gv_root, c_str("style"), c_str("bold"), c_str(""));

  // set layout
  gvLayout(gvc_, g, "dot");

  //
  gvRenderFilename(gvc_, g, "png", path.c_str());

  //
  gvFreeLayout(gvc_, g);

  agclose(g);
}

Agnode_s *TreeVisualizer::fill_graph(const TreeNode *node, Agraph_s *graph) {
  Agnode_s *gv_node = agnode(graph, unique_name(), true);
  agsafeset(gv_node, c_str("label"),  c_str(node->name()), c_str(""));
  for (const TreeNode *child : node->children()) {
    Agnode_s *gv_child = fill_graph(child, graph);
    agedge(graph, gv_node, gv_child, unique_name(), true);
  }

  if (node->children().empty()) {
    agsafeset(gv_node, c_str("style"), c_str("bold"), c_str(""));
  }

  return gv_node;
}

char *TreeVisualizer::unique_name() {
  return c_str(std::move(std::to_string(++current_id_)));
}
char *TreeVisualizer::c_str(std::string string) {
  if (string_index_.contains(string)) {
    return strings_[string_index_[string]].data();
  }
  strings_.emplace_back(std::move(string));
  string_index_[strings_.back()] = strings_.size() - 1;
  return strings_.back().data();
}

TreeVisualizer::TreeVisualizer() : current_id_{0}, strings_{}, string_index_{}, gvc_{nullptr} {
  gvc_ = gvContext();
}
TreeVisualizer::~TreeVisualizer() {
  gvFreeContext(gvc_);
}
} // ast