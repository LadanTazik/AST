package ast;

import java.io.IOException;
import utils.FileUtils;

/**
 *
 * @author tazik
 */
public class Main {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        System.out.println("Hello! This AST!\n");
        System.out.println("Test");
        String[] files = FileUtils.listSourceCodeFiles(args, ".java");
        for (String javaFile : files) {
            try {
                System.out.println("\n========================================\n");
                System.out.println("FILE: " + javaFile);
                 AbstractSyntaxTree ast = ASTbuilder.build(javaFile);
                ast.exportDOT("./");
            } catch (IOException ex) {
                System.err.println(ex);
            }
        }
    }
}
