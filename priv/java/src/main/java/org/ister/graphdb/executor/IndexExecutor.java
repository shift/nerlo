package org.ister.graphdb.executor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;

// TODO: fix IndexService -> IndexManager change

public class IndexExecutor extends AbstractGraphdbMsgExecutor {

	public Index<Node> nodeIndex;

	@Override
	protected boolean checkMsg(Msg msg) {
		return ((msg.has("op") &&
				 msg.has("id") &&
				 msg.has("key") &&
				 msg.has("value")));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		String op =  (String)msg.get("op");
		Long   id =  (Long)msg.get("id");
		String key = (String)msg.get("key");
		String val = (String)msg.get("value");

		if( op.equals("lookup") ) {
			Object result = lookup(key, val);
			Map<String, Object> map = Collections.singletonMap("result",result);
			return Msg.answer(self, MsgTag.OK, map, msg);
		} else if( op.equals("add") || op.equals("del") ) {
			op( op, id, key, val );
			Map<String, Object> map = Collections.singletonMap("result",
					(Object)"ok");
			return Msg.answer(self, MsgTag.OK, map, msg);
		} else {
			throw new RuntimeException("unknown operation: " + op);
		}
	}

	@Override
	protected String getId() {
		return "index";
	}

	private ArrayList<Long> lookup(String key, String val)
			throws ExecutorException, NotFoundException {
		ArrayList<Long> ids = new ArrayList<Long>();
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<org.neo4j.graphdb.Node> hits = nodeIndex.get(key, val);
			while( hits.hasNext() ) {
				Node node = (Node)hits.next();
				ids.add( node.getId() );
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return ids;
	}

	private void op(String op, Long id, String key, String val) throws ExecutorException {
		log.debug("op id:" + id);
		Transaction tx = this.db.beginTx();
		try {
			Node node = this.db.getNodeById(id.longValue());
			if (op.equals("add")) {
				nodeIndex.add(node, key, val);
			} else if (op.equals("del")) {
				nodeIndex.remove(node, key, val);
			}
			tx.success();
		} catch (NotFoundException e) {
			e.printStackTrace();
			tx.failure();
			throw new ExecutorException("vertex_not_found");
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
	}


}
