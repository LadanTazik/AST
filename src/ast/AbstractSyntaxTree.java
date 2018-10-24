package ast;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.jgrapht.graph.SimpleDirectedGraph;
import utils.*;

/**
 * @author tazik
 */
public class AbstractSyntaxTree extends SimpleDirectedGraph<ASNode, ASEdge> {

    public final String FILE_PATH;

    public AbstractSyntaxTree(String path) {
        super(ASEdge.class);
        this.FILE_PATH = path;
    }

    /**
     * Export this AST to specified file format.
     */
    public void export(String format, String outDir) throws FileNotFoundException {
        switch (format.toLowerCase()) {
            case "dot":
                exportDOT(outDir);
                break;
            case "json":
                //exportJSON(outDir);
                break;
        }
    }

    /*
     * Export this AST to JSON format. The JSON file will be saved inside the given directory path.
    public void exportJSON(String outDir) throws FileNotFoundException {
        String filename = FILE_PATH.substring(0, FILE_PATH.indexOf('.'));
        String filepath = outDir + filename + "-AST.json";
        try (PrintWriter json = new PrintWriter(filepath, "UTF-8")) {
            json.println("{\n  \"type\": \"AST\",");
            json.println("  \"file\": \"" + FILE_PATH + "\",");
            json.println("\n\n  \"nodes\": [");
            Map<ASNode, String> nodeIDs = new LinkedHashMap<>();
            int nodeCounter = 1;
            for (ASNode node : vertexSet()) {
                json.println("    {");
                String id = "n" + nodeCounter++;
                nodeIDs.put(node, id);
                json.println("      \"id\": \"" + id + "\",");
                json.println("      \"line\": \"" + node.getLineOfCode() + "\",");
                json.println("      \"code\": \"" + node.getValue().replace("\"", "\\\"") + "\"");
                json.println("    },");
            }
            json.println("  ],\n\n\n  \"edges\": [");
            int edgeCounter = 1;
            for (ASEdge edge : edgeSet()) {
                json.println("    {");
                String id = "e" + edgeCounter++;
                json.println("      \"id\": \"" + id + "\",");
                String src = nodeIDs.get(getEdgeSource(edge));
                json.println("      \"source\": \"" + src + "\",");
                String trgt = nodeIDs.get(getEdgeTarget(edge));
                json.println("      \"target\": \"" + trgt + "\",");
                json.println("      \"label\": \"" + edge.type.label + "\"");
                json.println("    },");
            }
            json.println("  ]\n}");
        } catch (UnsupportedEncodingException ex) {
            System.err.println(ex);
        }
        System.out.println("AST exported to: " + filepath);
    }
     */

    /**
     * Export this Control Flow Graph (AST) to DOT format. The DOT file will be saved inside the given directory. The DOT format is mainly aimed for visualization purposes.
     */
    public void exportDOT(String outDir) throws FileNotFoundException {
        String filename = new File(FILE_PATH).getName();
        filename = filename.substring(0, filename.lastIndexOf('.'));
        String filepath = outDir + filename + "-AST.dot";
        try (PrintWriter dot = new PrintWriter(filepath, "UTF-8")) {
            dot.println("digraph " + filename + " {\n");
            Map<ASNode, String> nodeNames = new LinkedHashMap<>();
            int nodeCounter = 1;
            for (ASNode node : vertexSet()) {
                String name = "n" + nodeCounter++;
                nodeNames.put(node, name);
                StringBuilder label = new StringBuilder("   [label=\"");
                //if (node.getLineOfCode() > 0) {
                //    label.append(node.getLineOfCode()).append(":  ");
                //}
                label.append(StringUtils.escape(node.toString())).append("\"];");
                dot.println("   " + name + label.toString());
            }
            dot.println();
            for (ASEdge edge : edgeSet()) {
                String src = nodeNames.get(getEdgeSource(edge));
                String trg = nodeNames.get(getEdgeTarget(edge));
                //if (edge.type.equals(ASEdge.Type.EPSILON)) {
                    dot.println("   " + src + " -> " + trg + ";");
                //} else {
                //    dot.println("   " + src + " -> " + trg + "   [label=\"" + edge.type + "\"];");
                //}
            }
            dot.println("\n}");
        } catch (UnsupportedEncodingException ex) {
            System.err.println(ex);
        }
        System.out.println("AST exported to: " + filepath);
    }

}
