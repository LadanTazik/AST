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
                   
			parentStack.push(classNode);
			visit(ctx.classBody());
			parentStack.pop();
            
           
            return null;
        }
        
        @Override
        public String visitClassBodyDeclaration(JavaParser.ClassBodyDeclarationContext ctx){
          
            //classBodyDeclaration
			//   :  ';'
			//   |  'static'? block
			//   |   modifier* memberDeclaration
            
            //
            if (ctx.memberDeclaration().fieldDeclaration() != null) {
            ASNode fieldsNode = new ASNode(ASNode.Type.FIELDS);
            ast.addVertex(fieldsNode);
            ast.addEdge(parentStack.peek(), fieldsNode);
            parentStack.push(fieldsNode);
            visit(ctx.memberDeclaration().fieldDeclaration());
            parentStack.pop();
            }
            
            //
            if (ctx.memberDeclaration().constructorDeclaration() !=null ){
            ASNode constructorsNode = new ASNode(ASNode.Type.CONSTRUCTORS);
            ast.addVertex(constructorsNode);
            ast.addEdge(parentStack.peek(), constructorsNode);
            parentStack.push(constructorsNode);
            visit(ctx.memberDeclaration().constructorDeclaration());
            parentStack.pop();
            }
            
            if(ctx.memberDeclaration().methodDeclaration() !=null){
               
                ASNode methodsNode = new ASNode(ASNode.Type.METHODS);
                ast.addVertex(methodsNode);
                ast.addEdge(parentStack.peek(), methodsNode);
                parentStack.push(methodsNode);
                visit(ctx.memberDeclaration().methodDeclaration());
                parentStack.pop();
            
            }
            
            if(ctx.memberDeclaration().classDeclaration() !=null){
            ASNode typesNode = new ASNode(ASNode.Type.TYPES);
            ast.addVertex(typesNode);
            ast.addEdge(parentStack.peek(), typesNode);
            parentStack.push(typesNode);
            //visit(ctx.memberDeclaration().);
            parentStack.pop();
            }
            
             return null;
        
        }
        
       @Override
       
       public String visitFieldDeclaration(JavaParser.FieldDeclarationContext ctx){
           
            //  fieldDeclaration
            //:   typeType variableDeclarators ';'
            //
            // variableDeclarators
            //:   variableDeclarator (',' variableDeclarator)*
            //
            // variableDeclarator
            // :   variableDeclaratorId ('=' variableInitializer)?
          
           for (JavaParser.VariableDeclaratorContext var: ctx.variableDeclarators().variableDeclarator()) {
               
                ASNode vars = new ASNode(ASNode.Type.VARIABLE);
                ast.addVertex(vars);
                ast.addEdge(parentStack.peek(), vars);
                //
                ASNode type = new ASNode(ASNode.Type.TYPE);
                ast.addVertex(type);
                type.setValue(ctx.typeType().getText());
                ast.addEdge(vars, type);
                //
                ASNode name = new ASNode(ASNode.Type.NAME);
                ast.addVertex(name);
                name.setValue(var.variableDeclaratorId().Identifier().getText());
                ast.addEdge(vars, name);
                //
                ASNode value= new ASNode(ASNode.Type.INITIALVALUE);
                ast.addVertex(value);
                value.setValue(var.variableInitializer().getText());
                ast.addEdge(vars, value);
				
			}
           return null;
            }
       
       @Override
       public String visitMethodDeclaration(JavaParser.MethodDeclarationContext ctx) {
            //  methodDeclaration
            //  :   (typeType|'void') Identifier formalParameters ('[' ']')* ('throws' qualifiedNameList)?  ( methodBody | ';')
            //
            //  formalParameters :  '(' formalParameterList? ')'
            //
            //  formalParameterList
            //    :   formalParameter (',' formalParameter)* (',' lastFormalParameter)?
            //    |   lastFormalParameter
            //
            //  formalParameter :  variableModifier* typeType variableDeclaratorId
            //
            //  lastFormalParameter :  variableModifier* typeType '...' variableDeclaratorId
               
            
              //
              String ret_type ;
              if(!"void".equals(ctx.getChild(0).getText()))
                  ret_type = ctx.getChild(0).getText();
              else
                  ret_type = "void";
              
                ASNode node = new ASNode(ASNode.Type.RETURN);
                node.setValue(ret_type);
                ast.addVertex(node);
                ast.addEdge(parentStack.peek(), node);
           
            //
			String methodId = ctx.Identifier().getText();
           // System.out.println(name);
            ASNode id = new ASNode(ASNode.Type.NAME);
            id.setValue(methodId);
            ast.addVertex(id);
            ast.addEdge(parentStack.peek(), id);
            
            //
            if (ctx.formalParameters().formalParameterList() != null){
                
                ASNode Params = new ASNode(ASNode.Type.PARAMETERS);
                ast.addVertex(Params);
                ast.addEdge(parentStack.peek(), Params);
                for (JavaParser.FormalParameterContext param : 
						ctx.formalParameters().formalParameterList().formalParameter()){
                    parentStack.push(Params);
                    ASNode Param= new ASNode(ASNode.Type.PARAMETER);
                    ast.addVertex(Param);
                    ast.addEdge(parentStack.peek(), Param);
                    //
                    ASNode type= new ASNode(ASNode.Type.TYPE);
                    ast.addVertex(type);
                    type.setValue(param.typeType().getText());
                    ast.addEdge(Param, type);
                    //
                    ASNode name= new ASNode(ASNode.Type.NAME);
                    ast.addVertex(name);
                    name.setValue(param.variableDeclaratorId().getText());
                    ast.addEdge(Param, name);
                    
                    if (ctx.formalParameters().formalParameterList().lastFormalParameter() != null) {
					type.setValue(ctx.formalParameters().formalParameterList().lastFormalParameter().typeType().getText());
                    name.setValue(ctx.formalParameters().formalParameterList().lastFormalParameter().variableDeclaratorId().getText());
				}
                    parentStack.pop();
            }
            
            }
           
            //
            ASNode methodBody = new ASNode(ASNode.Type.BLOCK);
            ast.addVertex(methodBody); 
            ast.addEdge(parentStack.peek(), methodBody);
            parentStack.push(methodBody);
            visit(ctx.methodBody());
            parentStack.pop();
              return null;
       
       }
       
        @Override
		public String visitLocalVariableDeclaration(JavaParser.LocalVariableDeclarationContext ctx) {
			// localVariableDeclaration :  variableModifier* typeType variableDeclarators
            //
            ASNode var = new ASNode(ASNode.Type.VARIABLE);
            ast.addVertex(var);
            ast.addEdge(parentStack.peek(), var);
            //
            ASNode type = new ASNode(ASNode.Type.TYPE);
            ast.addVertex(type);
            type.setValue(ctx.typeType().getText());
            ast.addEdge(var, type);
            //
            
            parentStack.push(var);
            visit(ctx.variableDeclarators());
            parentStack.pop();
            
			return null;
		}
        @Override
         public String visitVariableDeclarator(JavaParser.VariableDeclaratorContext ctx){
             //variableDeclarators
            //  :   variableDeclarator (',' variableDeclarator)*
            //variableDeclarator
            //  :   variableDeclaratorId ('=' variableInitializer)?
            
                ASNode name = new ASNode(ASNode.Type.NAME);
                ast.addVertex(name);
                name.setValue(ctx.variableDeclaratorId().getText());
                ast.addEdge(parentStack.peek(), name);
                //
                ASNode init = new ASNode(ASNode.Type.INITIALVALUE);
                ast.addVertex(init);
                init.setValue(ctx.variableInitializer().getText());
                ast.addEdge(parentStack.peek(), init);
                
         return null;
        }
         @Override
		public String visitStatementExpression(JavaParser.StatementExpressionContext ctx) {
			// statementExpression ';'
            ASNode node = new ASNode(ASNode.Type.STATEMENT);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
            //
            ASNode snode= new ASNode(ASNode.Type.EPSILON);
            ast.addVertex(snode);
            snode.setValue(ctx.expression().getText());
            ast.addEdge(node, snode);
            return null;
        }
       @Override
		public String visitIfStatement(JavaParser.IfStatementContext ctx) {
            // 'if' parExpression statement ('else' statement)?
            
            ASNode ifstat = new ASNode(ASNode.Type.IFSTATEMENT);
            ast.addVertex(ifstat);
            ast.addEdge(parentStack.peek(), ifstat);
            //
            ASNode cond= new ASNode(ASNode.Type.CONDITION);
            ast.addVertex(cond);
            ast.addEdge(ifstat, cond);
            ASNode node= new ASNode(ASNode.Type.EPSILON);
            ast.addVertex(node);
            node.setValue(ctx.parExpression().getText());
            ast.addEdge(cond, node);
            //
            
            ASNode st = new ASNode(ASNode.Type.BLOCK);
            ast.addVertex(st);
            ast.addEdge(ifstat, st);
            parentStack.push(st);
            visit(ctx.statement(0));
            parentStack.pop();
            //
            if(ctx.statement(1) !=null){
                ASNode elSe = new ASNode(ASNode.Type.ELSEIF);
                ast.addVertex(elSe);
                ast.addEdge(ifstat, elSe);
                parentStack.push(st);
                visit(ctx.statement(1));
                parentStack.pop();
                }
            
                       
        return null;
        }   
        
       @Override 
       public String visitForStatement (JavaParser.ForStatementContext ctx) {
           // 'for' '(' forControl ')' statement
           // forControl:   enhancedForControl
           //  |   forInit? ';' expression? ';' forUpdate?
           //enhancedForControl
           //  :   variableModifier* typeType variableDeclaratorId ':' expression
           // forInit:   localVariableDeclaration|   expressionList
           // forUpdate:   expressionList
           
          //
          ASNode node = new ASNode(ASNode.Type.FOR);
          ast.addVertex(node);
          ast.addEdge(parentStack.peek(), node);
          
          //
          ASNode for_control = new ASNode(ASNode.Type.FORCONTROOL);
          ast.addVertex(for_control);
          ast.addEdge(node, for_control);
          
          //
          ASNode block = new ASNode(ASNode.Type.BLOCK);
          ast.addVertex(block);
          ast.addEdge(node, block);
          
          // handling for-each loop
          if (ctx.forControl().enhancedForControl() !=null){
              
              //    
              ASNode init_type = new ASNode(ASNode.Type.TYPE);
              ASNode init_id = new ASNode(ASNode.Type.NAME);
              ast.addVertex(init_type);
              ast.addVertex(init_id);
              String typetype = ctx.forControl().enhancedForControl().typeType().getText();
              String id = ctx.forControl().enhancedForControl().variableDeclaratorId().getText();
              init_type.setValue(typetype);
              init_id.setValue(id);
              ast.addEdge(for_control, init_type);
              ast.addEdge(for_control, init_id);
              
              //
              ASNode expr = new ASNode(ASNode.Type.EPSILON);
              ast.addVertex(expr);
              expr.setValue(ctx.forControl().enhancedForControl().expression().getText());
              ast.addEdge(for_control, expr);
               
          }
          
          // handling classic for
          else{
              
              // for_init
              if (ctx.forControl().forInit() != null)
              {
                  ASNode for_init= new ASNode(ASNode.Type.FOR_INIT);
                  ast.addVertex(for_init);
                  for_init.setValue(ctx.forControl().forInit().getText());
                  ast.addEdge(for_control, for_init);  
              }
              //for_expression
              ASNode expr = new ASNode(ASNode.Type.CONDITION);
              ast.addVertex(expr);
              expr.setValue(ctx.forControl().expression().getText());
              ast.addEdge(for_control, expr);
              
              // for_update
              ASNode for_update = new ASNode(ASNode.Type.FOR_UPDATE);
              ast.addVertex(for_update);
              for_update.setValue(ctx.forControl().forUpdate().getText());
              ast.addEdge(for_control, for_update);
              
              }
          //
         parentStack.push(block);
         visit(ctx.statement());
         parentStack.pop();
         
            return null;
       }
    
       @Override
		public String visitWhileStatement(JavaParser.WhileStatementContext ctx) {
			// 'while' parExpression statement
            ASNode node= new ASNode(ASNode.Type.WHILE);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
             //
             ASNode cond = new ASNode(ASNode.Type.CONDITION);
             ast.addVertex(cond);
             ast.addEdge(node, cond);
             //
             ASNode st = new ASNode(ASNode.Type.BLOCK);
             ast.addVertex(st);
             ast.addEdge(node, st);
             //
             ASNode expr = new ASNode(ASNode.Type.EPSILON);
             ast.addVertex(expr);
             expr.setValue(ctx.parExpression().getText());
             ast.addEdge(cond, expr);
             //
             parentStack.push(st);
             visit(ctx.statement());
             parentStack.pop();
             
			return null;
		}
        
      
        @Override
		public String visitDoWhileStatement(JavaParser.DoWhileStatementContext ctx) {
            // 'do' statement 'while' parExpression ';'
            ASNode node= new ASNode(ASNode.Type.DOWHILE);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
             //
             ASNode st = new ASNode(ASNode.Type.BLOCK);
             ast.addVertex(st);
             ast.addEdge(node, st);
             //
             ASNode cond = new ASNode(ASNode.Type.CONDITION);
             ast.addVertex(cond);
             ast.addEdge(node, cond);
             //
             ASNode expr = new ASNode(ASNode.Type.EPSILON);
             ast.addVertex(expr);
             expr.setValue(ctx.parExpression().getText());
             ast.addEdge(cond, expr);
             //
             parentStack.push(st);
             visit(ctx.statement());
             parentStack.pop();
             
            return null;
        }
        
        @Override
		public String visitTryStatement(JavaParser.TryStatementContext ctx) {
			// 'try' block (catchClause+ finallyBlock? | finallyBlock)
            
           ASNode node = new ASNode(ASNode.Type.TRY);
           ast.addVertex(node);
           ast.addEdge(parentStack.peek(), node);
           //
           ASNode block = new ASNode(ASNode.Type.BLOCK);
           ast.addVertex(block);
           ast.addEdge(node, block);
           parentStack.push(block);
           visit(ctx.block());
           parentStack.pop();
           
          // finally-block
          //finallyBlock:   'finally' block
          if(ctx.finallyBlock() != null){
              ASNode fnode= new ASNode(ASNode.Type.FINALLY);
              ast.addVertex(fnode);
              ast.addEdge(node, fnode);
              parentStack.push(fnode);
              visit(ctx.finallyBlock().block());
              parentStack.pop();
          }
         //catchClause
         // :   'catch' '(' variableModifier* catchType Identifier ')' block
         if (ctx.catchClause().size()>0 && ctx.catchClause() != null ){
             
             for (JavaParser.CatchClauseContext cx: ctx.catchClause()) {
             ASNode cnode = new ASNode(ASNode.Type.CATCH);
             ast.addVertex(cnode);
             ast.addEdge(node, cnode);
             //
             ASNode type= new ASNode(ASNode.Type.TYPE);
             ast.addVertex(type);
             type.setValue(cx.catchType().getText());
             ast.addEdge(cnode, type);
             //
             ASNode id = new ASNode(ASNode.Type.NAME);
             ast.addVertex(id);
             id.setValue(cx.Identifier().getText());
             ast.addEdge(cnode, id);   
             //
             parentStack.push(cnode);
             visit(cx.block());
             parentStack.pop();
             }
         }
             return null;
            
        }
        @Override
		public String visitTryWithResourceStatement(JavaParser.TryWithResourceStatementContext ctx) {
			// 'try' resourceSpecification block catchClause* finallyBlock?
			// resourceSpecification :  '(' resources ';'? ')'
			// resources :  resource (';' resource)*
			// resource  :  variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
            //
            ASNode node = new ASNode(ASNode.Type.TRY);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(), node);
            //
            ASNode rnode = new ASNode(ASNode.Type.RESOURCE);
            for (JavaParser.ResourceContext rsrc: ctx.resourceSpecification().resources().resource()) {
				ASNode type = new ASNode(ASNode.Type.TYPES);
                ast.addVertex(type);
                type.setValue(rsrc.classOrInterfaceType().getText());
                ast.addEdge(rnode, type);
                //
                ASNode id = new ASNode(ASNode.Type.NAME);
                ast.addVertex(id);
                id.setValue(rsrc.variableDeclaratorId().getText());
                ast.addEdge(rnode, id);
                //
                ASNode init = new ASNode(ASNode.Type.INITIALVALUE);
                ast.addVertex(init);
                init.setValue(rsrc.expression().getText());
                ast.addEdge(rnode, init);	
			}
               ASNode block = new ASNode(ASNode.Type.BLOCK);
           ast.addVertex(block);
           ast.addEdge(node, block);
           parentStack.push(block);
           visit(ctx.block());
           parentStack.pop();
           
          // finally-block
          //finallyBlock:   'finally' block
          if(ctx.finallyBlock() != null){
              ASNode fnode= new ASNode(ASNode.Type.FINALLY);
              ast.addVertex(fnode);
              ast.addEdge(node, fnode);
              parentStack.push(fnode);
              visit(ctx.finallyBlock().block());
              parentStack.pop();
          }
         //catchClause
         // :   'catch' '(' variableModifier* catchType Identifier ')' block
         if (ctx.catchClause().size()>0 && ctx.catchClause() != null ){
             
             for (JavaParser.CatchClauseContext cx: ctx.catchClause()) {
             ASNode cnode = new ASNode(ASNode.Type.CATCH);
             ast.addVertex(cnode);
             ast.addEdge(node, cnode);
             //
             ASNode type= new ASNode(ASNode.Type.TYPE);
             ast.addVertex(type);
             type.setValue(cx.catchType().getText());
             ast.addEdge(cnode, type);
             //
             ASNode id = new ASNode(ASNode.Type.NAME);
             ast.addVertex(id);
             id.setValue(cx.Identifier().getText());
             ast.addEdge(cnode, id);   
             //
             parentStack.push(cnode);
             visit(cx.block());
             parentStack.pop();
             }
         }
            return null;
        }
        
        @Override
		public String visitSwitchStatement(JavaParser.SwitchStatementContext ctx) {
			// 'switch' parExpression '{' switchBlockStatementGroup* switchLabel* '}'
            
            ASNode node = new ASNode (ASNode.Type.SWITCH);
            ast.addVertex(node);
            ast.addEdge(parentStack.peek(),node);
            //
            ASNode expr = new ASNode (ASNode.Type.EXPRESSION);
            ast.addVertex(expr);
            expr.setValue(ctx.parExpression().getText());
            ast.addEdge(node,expr);
            
            //
            //parentStack.push(node);
            for (JavaParser.SwitchLabelContext lb: ctx.switchLabel()){
                //switchBlockStatementGroup  :   switchLabel+ blockStatement+
                //    switchLabel :   'case' constantExpression ':'
                              //  |   'case' enumConstantName ':'
               //                 |   'default' ':'
    ;
                
                if(lb.constantExpression() != null || lb.enumConstantName()!=null)
                {
                    ASNode cnode = new ASNode (ASNode.Type.CASE);
                    ast.addVertex(cnode);
                    ast.addEdge(node, cnode);
                  //  
                    ASNode val = new ASNode(ASNode.Type.INITIALVALUE);
                    ast.addVertex(val);
                    if(lb.constantExpression() != null)
                     val.setValue(lb.constantExpression().getText());
                        else if(lb.enumConstantName()!=null)
                                val.setValue(lb.enumConstantName().getText());
                    ast.addEdge(cnode, val);
                    parentStack.push(cnode);
                }
                    else
                   {
                      ASNode dnode = new ASNode (ASNode.Type.DEFAULT);
                       ast.addVertex(dnode);
                       ast.addEdge(node, dnode); 
                       parentStack.push(dnode);
                   }
                
                ASNode block = new ASNode(ASNode.Type.BLOCK);
                
                for (JavaParser.SwitchBlockStatementGroupContext grp: ctx.switchBlockStatementGroup()) {
                    for (JavaParser.BlockStatementContext blk: grp.blockStatement()){
                        
                        ast.addVertex(block);
                        ast.addEdge(parentStack.peek(), block);
                        visit(blk);
                        parentStack.pop();
                    }
		
            }
        return null;
        
        }
     }
        
        
 }   

