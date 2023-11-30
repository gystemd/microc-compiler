struct Node {
    int v;
    struct Node* left;
    struct Node* right;
};

void print_tree_in_dfs(struct Node *root){
    if(root != NULL){
        print_tree_in_dfs((*root).left);
        print((*root).v);
        print_tree_in_dfs((*root).right);
        
    }
}
int main(){
    struct Node l1;
    struct Node l2;
    struct Node l3;
    struct Node l4;
    struct Node n1;
    struct Node n2;
    struct Node root;
    
    l1.left = NULL;
    l1.right = NULL;
    l1.v = 4;
    
    l2.left = NULL;
    l2.right = NULL;
    l2.v = 5;

    l3.left = NULL;
    l3.right = NULL;
    l3.v = 6;

    l4.left = NULL;
    l4.right = NULL;
    l4.v = 7;

    n1.left = &l1;
    n1.right = &l2;
    n1.v=2;

    n2.left=&l3;
    n2.right=&l4;
    n2.v=3;

    root.v = 1;
    root.left = &n1;
    root.right = &n2;
    print_tree_in_dfs(&root);
    return 0;
}