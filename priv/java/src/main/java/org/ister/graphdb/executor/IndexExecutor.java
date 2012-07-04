package org.ister.graphdb.executor;

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

		if( op.equals("lookup") || op.equals("lookup_one") ) {
			Object result = null;
			if( op.equals("lookup_one") ) {
				result = lookupOne(key, val);
			} else {
				result = lookup(key, val);
			}

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

	private Object lookupOne(String key, String val) throws ExecutorException {
		long id = -1L;
		IndexHits<Node> hits = nodeIndex.get(key, val);
		if( hits.size() < 1 )
			throw new NotFoundException("no_vertex_found");
		id = hits.getSingle().getId();
		return new Long(id);
	}

	private Object lookup(String key, String val)
			throws ExecutorException {
		long id = -1L;
		Transaction tx = this.db.beginTx();
		try {
//			IndexHits<org.neo4j.graphdb.Node> hits = this.index.getNodes(key, val);
//			if (hits.size() < 1) {
//				throw new ExecutorException("no_vertex_found");
//			} else if (hits.size() > 1) {
//				throw new ExecutorException("multiple_vertices_found");
//			}
//			id = hits.next().getId();
			tx.success();
//		} catch (ExecutorException e) {
//			throw e;
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return new Long(id);
	}

	private void op(String op, Long id, String key, String val) throws ExecutorException {
		log.debug("op id:" + id);
		Transaction tx = this.db.beginTx();
		try {
			Node node = this.db.getNodeById(id.longValue());
			if (op.equals("add")) {
				this.nodeIndex.add(node, key, val);
			} else if (op.equals("del")) {
				this.nodeIndex.remove(node, key, val);
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
