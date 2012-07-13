package org.ister.graphdb.executor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.nerlo.ExecutorException;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Relationship;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.Index;
import org.neo4j.graphdb.index.IndexHits;
import org.neo4j.graphdb.index.RelationshipIndex;

// TODO: this is some bad code.  fix the DRY, fix the if then else shit, etc.

public class IndexExecutor extends AbstractGraphdbMsgExecutor {

	private Index<Node> nodeIndex;
	private RelationshipIndex edgeIndex;

	@Override
	protected boolean checkMsg(Msg msg) {
		return ((msg.has("op") && msg.has("type") && msg.has("name")
				&& msg.has("id") && msg.has("key") && msg.has("value")) && (type_is_valid((String) msg
				.get("type"))));
	}

	@Override
	protected Msg execMsg(Msg msg) throws ExecutorException {
		String op = (String) msg.get("op");
		String type = (String) msg.get("type");
		String name = (String) msg.get("name");
		Long id = (Long) msg.get("id");
		String key = (String) msg.get("key");
		String val = (String) msg.get("value");

		Object result = null;

		try {
			// create_or_set_index for all ops
			result = create_or_set_index(type, name);

			if (op.equalsIgnoreCase("create")) {
				// do nothing, because we already did it above
			} else if (op.equalsIgnoreCase("delete")) {
				result = delete_index(type, name);
			} else if (op.equalsIgnoreCase("lookup")) {
				if (type.equalsIgnoreCase("node")) {
					result = lookup_node(key, val);
				} else {
					result = lookup_edge(key, val);
				}
			} else if (op.equalsIgnoreCase("query")) {
				if (type.equalsIgnoreCase("node")) {
					result = query_node(val);
				} else {
					result = query_edge(val);
				}
			} else if (op.equalsIgnoreCase("add") || op.equalsIgnoreCase("del")) {
				if (type.equalsIgnoreCase("node")) {
					result = op_node(op, id, key, val);
				} else {
					result = op_edge(op, id, key, val);
				}
			} else {
				throw new RuntimeException("unknown operation: " + op);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		Map<String, Object> map = Collections.singletonMap("result", result);
		return Msg.answer(self, MsgTag.OK, map, msg);
	}

	@Override
	protected String getId() {
		return "index";
	}

	private boolean type_is_valid(String type) {
		return type.equalsIgnoreCase("node") || type.equalsIgnoreCase("edge");
	}

	private Object create_or_set_index(String type, String name)
			throws ExecutorException {
		if (type.equalsIgnoreCase("node")) {
			nodeIndex = index_mgr.forNodes(name);
		} else if (type.equalsIgnoreCase("edge")) {
			edgeIndex = index_mgr.forRelationships(name);
		} else {
			throw new ExecutorException("bad index type: " + type);
		}
		return (Object) "ok";
	}

	private Object delete_index(String type, String name)
			throws ExecutorException {
		boolean ret = false;
		if (type.equalsIgnoreCase("node")) {
			nodeIndex.delete();
			ret = true;
		} else if (type.equalsIgnoreCase("edge")) {
			edgeIndex.delete();
			ret = true;
		} else {
			throw new ExecutorException("bad index type: " + type);
		}
		return ret;
	}

	private ArrayList<Long> lookup_node(String key, String val)
			throws ExecutorException, NotFoundException {
		ArrayList<Long> ids = new ArrayList<Long>();
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<Node> hits = nodeIndex.get(key, val);
			while (hits.hasNext()) {
				Node node = (Node) hits.next();
				ids.add(node.getId());
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			e.printStackTrace();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return ids;
	}

	private ArrayList<Long> lookup_edge(String key, String val)
			throws ExecutorException, NotFoundException {
		ArrayList<Long> ids = new ArrayList<Long>();
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<Relationship> hits = edgeIndex.get(key, val);
			while (hits.hasNext()) {
				Relationship edge = (Relationship) hits.next();
				ids.add(edge.getId());
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			e.printStackTrace();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return ids;
	}

	private ArrayList<Long> query_node(String query) throws ExecutorException,
			NotFoundException {
		ArrayList<Long> ids = new ArrayList<Long>();
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<Node> hits = nodeIndex.query(query);
			while (hits.hasNext()) {
				Node node = (Node) hits.next();
				ids.add(node.getId());
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			e.printStackTrace();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return ids;
	}

	private ArrayList<Long> query_edge(String query) throws ExecutorException,
			NotFoundException {
		ArrayList<Long> ids = new ArrayList<Long>();
		Transaction tx = this.db.beginTx();
		try {
			IndexHits<Relationship> hits = edgeIndex.query(query);
			while (hits.hasNext()) {
				Relationship edge = (Relationship) hits.next();
				ids.add(edge.getId());
			}
			tx.success();
		} catch (Exception e) {
			log.error("could not operate on index: " + e.toString());
			tx.failure();
			e.printStackTrace();
			throw new ExecutorException("could_not_operate_on_index");
		} finally {
			tx.finish();
		}
		return ids;
	}

	private Object op_node(String op, Long id, String key, String val)
			throws ExecutorException {
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

		return (Object) "ok";
	}

	private Object op_edge(String op, Long id, String key, String val)
			throws ExecutorException {
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

		return (Object) "ok";
	}

}
