package org.ister.graphdb.executor;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.apache.log4j.Logger;
import org.ister.ej.Msg;
import org.ister.ej.MsgRef;
import org.ister.ej.MsgTag;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.Index;
import org.neo4j.kernel.EmbeddedGraphDatabase;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IndexExecutorTest {

	private static Logger log;

	private GraphDatabaseService db;
	private static final String DB_PATH = "/tmp/neo4j-test";
	private static final String NODEINDEX_NAME = "usernames";
	private IndexExecutor ie;

	private final OtpErlangString calltag = new OtpErlangString(MsgTag.CALL);

	private final OtpErlangPid pid = new OtpErlangPid("foo",0,0,0);
	private final MsgRef ref = new MsgRef(pid, new OtpErlangRef("dummy", 0, 0));

	private final OtpErlangTuple handler = getTwoTuple("handler",
			new OtpErlangAtom("graphdb") );
	private final OtpErlangTuple call = getTwoTuple("call",
			new OtpErlangAtom("index") );

	private ArrayList<Node> testNodes;

	@Before
	public void setUp() throws Exception {
		log = Logger.getLogger(this.getClass());
		testNodes = new ArrayList<Node>();

		// startup db
		db = new EmbeddedGraphDatabase( DB_PATH );
		ie = new IndexExecutor();
		ie.init(pid, db);

		makeTestVertex("username", "boorad");
	}

	@After
	public void tearDown() throws Exception {
		Transaction tx = db.beginTx();
		try {
			// TODO: delete node index;
			for( Node n : testNodes ) {
				n.delete();
			}
			tx.success();
		} finally {
			tx.finish();
		}
		db.shutdown();
	}

	@Test
	public void testIndexLookupOne() throws Exception {
		OtpErlangTuple tuple = getLookupPayload("username", "boorad");
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
		assertEquals("index lookup",
				     "{ok,[{result,[1]}]}", resp.getMsg().toString());
    }

	@Test
	public void testIndexLookupNotFound() throws Exception {
		OtpErlangTuple tuple = getLookupPayload("username", "notboorad");
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
		assertEquals("index lookup not found",
					 "{ok,[{result,[]}]}", resp.getMsg().toString());
    }

	@Test
	public void testIndexLookupMultiple() throws Exception {
		makeTestVertex("username", "ingo");
		makeTestVertex("username", "boorad");  // a dupe

		OtpErlangTuple tuple = getLookupPayload("username", "boorad");
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
		String strResp = resp.getMsg().toString();
		assert(strResp.equalsIgnoreCase("{ok,[{result,[1,3]}]}") ||
			   strResp.equalsIgnoreCase("{ok,[{result,[3,1]}]}")
			  );
    }

	@Test
	public void testAddOne() throws Exception {
		OtpErlangTuple tuple = getPayload("add","node",1L,"username","boorad");
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
		assertEquals("add one",
					 "{ok,[{result,\"ok\"}]}", resp.getMsg().toString());
	}


	// TODO: put these in TestUtils somewhere shared
	private OtpErlangTuple getTwoTuple(String a, OtpErlangObject b) {
		OtpErlangObject[] list = new OtpErlangObject[]{
				new OtpErlangAtom(a), b };
		OtpErlangTuple handler = new OtpErlangTuple(list);
		return handler;
	}

	private OtpErlangTuple getLookupPayload(String k, String v) {
		return getPayload("lookup", "node", -1L, k, v);
	}

	private OtpErlangTuple getPayload(String o, String t,
			Long l, String k, String v) {

		OtpErlangTuple op = getTwoTuple( "op", new OtpErlangAtom(o) );
		OtpErlangTuple type = getTwoTuple("type", new OtpErlangAtom(t));
		OtpErlangTuple name = getTwoTuple("name",
				new OtpErlangAtom(NODEINDEX_NAME));
		OtpErlangTuple id = getTwoTuple( "id", new OtpErlangLong(l) );
		OtpErlangTuple key = getTwoTuple( "key", new OtpErlangAtom(k) );
		OtpErlangTuple val = getTwoTuple( "value", new OtpErlangString(v) );

		OtpErlangObject[] list = new OtpErlangObject[]{
				handler, call, op, type, name, id, key, val};
		OtpErlangList payload = new OtpErlangList(list);
		OtpErlangObject[] os  = new OtpErlangObject[]{calltag, payload};
		OtpErlangTuple tuple = new OtpErlangTuple(os);
		return tuple;
	}

	private void makeTestVertex(String key, String val) {
		Transaction tx = db.beginTx();
		try {
			// make test vertex
			Node node = db.createNode();
	        node.setProperty(key, val);
	        Index<Node> nodeIndex = db.index().forNodes(NODEINDEX_NAME);
	        nodeIndex.add(node, key, node.getProperty(key));
	        testNodes.add(node);
	        tx.success();
		} catch( Exception e ) {
			log.error("Error making test vertex: " + e);
			e.printStackTrace();
		} finally {
			tx.finish();
		}
	}
}
