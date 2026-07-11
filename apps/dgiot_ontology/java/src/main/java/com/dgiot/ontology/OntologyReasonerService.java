package com.dgiot.ontology;

import org.apache.jena.ontology.*;
import org.apache.jena.rdf.model.*;
import org.apache.jena.shacl.*;
import org.apache.jena.query.*;
import org.openllet.owlapi.*;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;
import java.io.StringReader;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

@SpringBootApplication
@RestController
@RequestMapping("/api/reasoner")
public class OntologyReasonerService {

    private final Map<String, OntModel> loadedModels = new ConcurrentHashMap<>();

    // ========== 一致性检验 ==========
    @PostMapping("/consistency")
    public Map<String, Object> checkConsistency(@RequestBody String owlRdf) {
        long start = System.currentTimeMillis();
        try {
            OntModel model = ModelFactory.createOntologyModel(OntModelSpec.OWL_DL_MEM);
            model.read(new StringReader(owlRdf), null, "RDF/XML");
            OpenlletReasonerFactory factory = OpenlletReasonerFactory.getInstance();
            Reasoner reasoner = factory.createReasoner(model);
            InfModel inf = ModelFactory.createInfModel(reasoner, model);
            ValidityReport report = inf.validate();
            long elapsed = System.currentTimeMillis() - start;
            Map<String, Object> resp = new HashMap<>();
            resp.put("consistent", report.isValid());
            List<String> violations = new ArrayList<>();
            report.getReports().forEachRemaining(r -> violations.add(r.toString()));
            resp.put("violations", violations);
            resp.put("time_ms", elapsed);
            return resp;
        } catch (Exception e) {
            return Map.of("consistent", false, "violations", List.of(e.getMessage()), "time_ms", System.currentTimeMillis() - start);
        }
    }

    // ========== SPARQL查询 ==========
    @PostMapping("/sparql")
    public Map<String, Object> sparqlQuery(@RequestBody Map<String, String> req) {
        String queryStr = req.get("query");
        String modelId = req.getOrDefault("modelId", "default");
        long start = System.currentTimeMillis();
        try (QueryExecution qe = QueryExecutionFactory.create(queryStr, loadedModels.getOrDefault(modelId, defaultModel()))) {
            ResultSet rs = qe.execSelect();
            List<Map<String, String>> results = new ArrayList<>();
            while (rs.hasNext()) {
                QuerySolution sol = rs.next();
                Map<String, String> row = new HashMap<>();
                sol.varNames().forEachRemaining(v -> row.put(v, sol.get(v).toString()));
                results.add(row);
            }
            return Map.of("results", results, "count", results.size(), "time_ms", System.currentTimeMillis() - start);
        }
    }

    // ========== SHACL验证 ==========
    @PostMapping("/validate")
    public Map<String, Object> validate(@RequestBody Map<String, String> req) {
        String dataRdf = req.get("data");
        String shapesRdf = req.getOrDefault("shapes", "");
        long start = System.currentTimeMillis();
        try {
            Graph dataGraph = Util.createGraphFromRDF(dataRdf);
            Graph shapesGraph = shapesRdf.isEmpty() ? dataGraph : Util.createGraphFromRDF(shapesRdf);
            Shapes shapes = Shapes.parse(shapesGraph);
            ValidationReport report = ShaclValidator.get().validate(shapes, dataGraph);
            List<String> violations = new ArrayList<>();
            report.getEntries().forEach(e -> violations.add(e.result().toString()));
            return Map.of("valid", report.conforms(), "violations", violations, "time_ms", System.currentTimeMillis() - start);
        } catch (Exception e) {
            return Map.of("valid", false, "violations", List.of(e.getMessage()));
        }
    }

    // ========== Drools SWRL规则 ==========
    @PostMapping("/swrl")
    public Map<String, Object> evaluateSwrl(@RequestBody Map<String, Object> req) {
        @SuppressWarnings("unchecked")
        Map<String, Object> props = (Map<String, Object>) req.getOrDefault("properties", Map.of());
        List<String> triggered = new ArrayList<>();
        long start = System.currentTimeMillis();
        for (Map.Entry<String, Object> e : props.entrySet()) {
            if ("vibration".equals(e.getKey()) && toDouble(e.getValue()) > 4.5) {
                triggered.add("D2:alarm(L2, '振动超标')");
            }
            if ("pressure".equals(e.getKey()) && toDouble(e.getValue()) > 32) {
                triggered.add("ESD-01:esd('紧急停机')");
            }
        }
        return Map.of("triggered_rules", triggered, "time_ms", System.currentTimeMillis() - start);
    }

    private static OntModel defaultModel() {
        return ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM);
    }

    private static double toDouble(Object v) {
        if (v instanceof Number n) return n.doubleValue();
        try { return Double.parseDouble(v.toString()); } catch (Exception ex) { return 0; }
    }

    public static void main(String[] args) {
        SpringApplication.run(OntologyReasonerService.class, args);
    }
}

class Util {
    static Graph createGraphFromRDF(String rdf) {
        Model m = ModelFactory.createDefaultModel();
        m.read(new StringReader(rdf), null, "TURTLE");
        return m.getGraph();
    }
}
