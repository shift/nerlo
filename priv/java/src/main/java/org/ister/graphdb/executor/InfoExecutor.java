package org.ister.graphdb.executor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.cypher.internal.symbols.RelationshipType;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseFactory;
import org.neo4j.graphdb.index.Index;
import org.neo4j.kernel.EmbeddedGraphDatabase;
import org.neo4j.kernel.impl.core.NodeManager;
import org.neo4j.kernel.KernelData;

@SuppressWarnings("deprecation")
public class InfoExecutor extends AbstractGraphdbMsgExecutor {

	@Override
	protected boolean checkMsg(Msg msg) {
		return msg.has("item");
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		Object info = info((String)msg.get("item"));
		if (info == null) {
			throw new ExecutorException("could_not_get_info");
		}
		Map<String, Object> map = new HashMap<String, Object>(1);
		map.put("result", info);
		return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
	}

	@Override
	protected String getId() {
		return "info";
	}
    private static final String DB_PATH = "neo4j-store";
    private static GraphDatabaseService graphDb;
    private static Index<Node> nm;
    private Object info(String item) {
		Object info = null;
		graphDb = new GraphDatabaseFactory().newEmbeddedDatabase( DB_PATH );
        graphDb.index().forNodes( "nodes" );
		
		Transaction tx = graphDb.beginTx();
		try {
			if (item.equals("order")) {
				long count = ((NodeManager) nm).getNumberOfIdsInUse(Node.class);
				info = new Long(count);
			} else if (item.equals("size")) {
				long count  = ((NodeManager) nm).getNumberOfIdsInUse(Relationship.class);
				info = new Long(count);
			} else if (item.equals("types")) {
				ArrayList<String> list = new ArrayList<String>();
		        for (org.neo4j.graphdb.RelationshipType t :  graphDb.getRelationshipTypes()) {
		            list.add(t.toString());
		        }
		        info = list;
			} else {
				throw new Exception("unknown item: " + item);
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not get info for " + item);
			tx.failure();
		} finally {
			tx.finish();
		}
		return info;
	}

}
