package ast;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayDeque;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import parser.JavaBaseVisitor;
import parser.JavaParser;
import parser.JavaLexer;
import java.util.Deque;


/**
 * @author tazik
 */
public class ASTbuilder {

    public static AbstractSyntaxTree build(String javaFile) throws IOException {
        return build(new File(javaFile));
    }

    /**
     * ‌
     */
    public static AbstractSyntaxTree build(File javaFile) throws IOException {
        if (!javaFile.getName().endsWith(".java"))
            throw new IOException("Not a Java File!");
        InputStream inFile = new FileInputStream(javaFile);
        ANTLRInputStream input = new ANTLRInputStream(inFile);
        JavaLexer lexer = new JavaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokens);
        ParseTree tree = parser.compilationUnit();
        ASTVisitor visitor = new ASTVisitor(javaFile.getPath());
        return visitor.build(tree);
    }


    private static class ASTVisitor extends JavaBaseVisitor<String> {

        private final String FILE_PATH;
        private AbstractSyntaxTree ast;
		private Deque<ASNode> parentStack;
        private String typeModifier;

        public ASTVisitor(String path) {
            FILE_PATH = path;
            parentStack = new ArrayDeque<>();
        }
        
        public AbstractSyntaxTree build(ParseTree tree) {
            ast = new AbstractSyntaxTree(FILE_PATH);
            ASNode root = new ASNode(ASNode.Type.ROOT);
            root.setValue(new File(FILE_PATH).getName());
            ast.addVertex(root);
            //
            JavaParser.CompilationUnitContext parseRoot = (JavaParser.CompilationUnitContext) tree;
            parentStack.push(root);
            if (parseRoot.packageDeclaration() != null)
                visit(parseRoot.packageDeclaration());
            //
            if (parseRoot.importDeclaration().size() > 0) {
                ASNode imports = new ASNode(ASNode.Type.IMPORTS);
                ast.addVertex(imports);
                ast.addEdge(root, imports);
                parentStack.push(imports);
                for (JavaParser.ImportDeclarationContext importCtx : parseRoot.importDeclaration())
                    visit(importCtx);
                parentStack.pop();
            }
            //
            for (JavaParser.TypeDeclarationContext typeDecCtx: parseRoot.typeDeclaration())
                visit(typeDecCtx);
            parentStack.pop();
            return ast;
        }

		@Override
		public String visitPackageDeclaration(JavaParser.PackageDeclarationContext ctx) {
			// packageDeclaration :  annotation* 'package' qualifiedName ';'
			String packageName = ctx.qualifiedName().getText();
            ASNode node = new ASNode(ASNode.Type.PACKAGE);
            node.setValue(packageName);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
			return null;
		}
        
		@Override
		public String visitImportDeclaration(JavaParser.ImportDeclarationContext ctx) {
			// importDeclaration :  'import' 'static'? qualifiedName ('.' '*')? ';'
			String qualifiedName = ctx.qualifiedName().getText();
			int last = ctx.getChildCount() - 1;
			if (ctx.getChild(last - 1).getText().equals("*")
					&& ctx.getChild(last - 2).getText().equals("."))
				qualifiedName += ".*";
            ASNode node = new ASNode(ASNode.Type.NAME);
            node.setValue(qualifiedName);
			ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
			return null;
		}
        
        @Override
        public String visitTypeDeclaration(JavaParser.TypeDeclarationContext ctx) {
            // typeDeclaration
            //    :   classOrInterfaceModifier* classDeclaration
            //    |   classOrInterfaceModifier* enumDeclaration
            //    |   classOrInterfaceModifier* interfaceDeclaration
            //    |   classOrInterfaceModifier* annotationTypeDeclaration
            //    |   ';'
            typeModifier = "";
            for (JavaParser.ClassOrInterfaceModifierContext modifierCtx: ctx.classOrInterfaceModifier())
                typeModifier += modifierCtx.getText() + " ";
            typeModifier = typeModifier.trim();
            visitChildren(ctx);
            return null;
        }
		
		@Override
		public String visitClassDeclaration(JavaParser.ClassDeclarationContext ctx) {
			// classDeclaration 
			//   :  'class' Identifier typeParameters? 
			//      ('extends' typeType)? ('implements' typeList)? classBody
            ASNode classNode = new ASNode(ASNode.Type.CLASS);
            ast.addVertex(classNode);
            ast.addEdge(parentStack.peek(), classNode);
            //
            ASNode modifierNode = new ASNode(ASNode.Type.MODFIER);
            modifierNode.setValue(typeModifier);
            ast.addVertex(modifierNode);
            ast.addEdge(classNode, modifierNode);
            //
			ASNode nameNode = new ASNode(ASNode.Type.NAME);
            String className = ctx.Identifier().getText();
			if(ctx.typeParameters() != null)
				className += ctx.typeParameters().getText();
            nameNode.setValue(className);
            ast.addVertex(nameNode);
            ast.addEdge(classNode, nameNode);
            //
			if (ctx.typeType() != null) {
				String extend = ctx.typeType().getText();
                ASNode extendsNode = new ASNode(ASNode.Type.EXTENDS);
                extendsNode.setValue(extend);
                ast.addVertex(extendsNode);
                ast.addEdge(classNode, extendsNode);
            }
            //
			if (ctx.typeList() != null) {
                ASNode implementsNode = new ASNode(ASNode.Type.IMPLEMENTS);
                ast.addVertex(implementsNode);
				for (JavaParser.TypeTypeContext type: ctx.typeList().typeType()) {
                    ASNode node = new ASNode(ASNode.Type.NAME);
                    node.setValue(type.getText());
                    ast.addVertex(node);
                    ast.addEdge(implementsNode, node);
                }
			}
            //
			parentStack.push(classNode);
			visit(ctx.classBody());
			parentStack.pop();
            return null;
        }
        
        @Override
        public String visitClassBody(JavaParser.ClassBodyContext ctx) {
            ASNode node = new ASNode(ASNode.Type.BLOCK);
            node.setValue("{ Class Body }");
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
            return null;
        }
    }
}
