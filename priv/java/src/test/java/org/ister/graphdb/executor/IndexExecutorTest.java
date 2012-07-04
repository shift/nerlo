package org.ister.graphdb.executor;

import static org.junit.Assert.assertTrue;

import org.apache.log4j.Logger;
import org.ister.ej.Msg;
import org.ister.ej.MsgRef;
import org.ister.ej.MsgTag;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.index.IndexManager;
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
	private IndexManager mgr;
	private static final String DB_PATH = "/tmp/neo4j-test";
	private IndexExecutor ie;

	private final OtpErlangString calltag = new OtpErlangString(MsgTag.CALL);

	private final OtpErlangPid pid = new OtpErlangPid("foo",0,0,0);
	private final MsgRef ref = new MsgRef(pid, new OtpErlangRef("dummy", 0, 0));

	private final OtpErlangTuple handler = getTwoTuple("handler", new OtpErlangAtom("graphdb") );
	private final OtpErlangTuple call = getTwoTuple("call", new OtpErlangAtom("index") );

	private Node testNode;

	@Before
	public void setUp() throws Exception {
		log = Logger.getLogger(this.getClass());

		// startup db
		db = new EmbeddedGraphDatabase( DB_PATH );
		mgr = db.index();
		ie = new IndexExecutor();
		ie.init(pid, db, mgr);
		ie.nodeIndex = mgr.forNodes("usernames");

		makeTestVertex();
	}

	@After
	public void tearDown() throws Exception {
		Transaction tx = db.beginTx();
		try {
			ie.nodeIndex.delete();
			this.testNode.delete();
			tx.success();
		} finally {
			tx.finish();
		}
		db.shutdown();
	}

	@Test
	public void testIndexLookupOne() throws Exception {
		OtpErlangTuple op = getTwoTuple( "op", new OtpErlangAtom("lookup_one") );
		OtpErlangTuple id = getTwoTuple( "id", new OtpErlangLong(-1L) );
		OtpErlangTuple key = getTwoTuple( "key", new OtpErlangAtom("username") );
		OtpErlangTuple val = getTwoTuple( "value", new OtpErlangString("boorad") );

		OtpErlangTuple tuple = getPayload(new OtpErlangObject[]{
				handler, call, op, id, key, val});
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
		assertTrue(resp.getMsg().toString().equals("{ok,[{result,1}]}"));
    }

	@Test(expected = NotFoundException.class)
	public void testIndexLookupOneNotFound() throws Exception {
		OtpErlangTuple op = getTwoTuple( "op", new OtpErlangAtom("lookup_one") );
		OtpErlangTuple id = getTwoTuple( "id", new OtpErlangLong(-1L) );
		OtpErlangTuple key = getTwoTuple( "key", new OtpErlangAtom("username") );
		OtpErlangTuple val = getTwoTuple( "value", new OtpErlangString("notboorad") );

		OtpErlangTuple tuple = getPayload(new OtpErlangObject[]{
				handler, call, op, id, key, val});
		Msg msg = new Msg(pid, ref, tuple);
		assertTrue( ie.checkMsg(msg) );
		Msg resp = ie.execMsg(msg);
    }





	// TODO: put these in TestUtils somewhere shared
	private OtpErlangTuple getTwoTuple(String a, OtpErlangObject b) {
		OtpErlangObject[] list = new OtpErlangObject[]{
				new OtpErlangAtom(a), b };
		OtpErlangTuple handler = new OtpErlangTuple(list);
		return handler;
	}

	private OtpErlangTuple getPayload(OtpErlangObject[] list) {
		OtpErlangList payload = new OtpErlangList(list);
		OtpErlangObject[] os  = new OtpErlangObject[]{calltag, payload};
		OtpErlangTuple tuple = new OtpErlangTuple(os);
		return tuple;
	}

	private void makeTestVertex() {
		Transaction tx = db.beginTx();
		try {
			// make test vertex
			Node node = db.createNode();
	        node.setProperty("username", "boorad");
	        ie.nodeIndex.add(node, "username", node.getProperty("username"));
	        this.testNode = node;
	        tx.success();
		} catch( Exception e ) {
			log.error("Error making test vertex: " + e);
			e.printStackTrace();
		} finally {
			tx.finish();
		}
	}
}
