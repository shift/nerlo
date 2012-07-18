package org.ister.graphdb;

import org.ister.ej.Msg;
import org.ister.ej.MsgRef;
import org.ister.ej.MsgTag;
import org.ister.nerlo.EjMsgHandler;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class EjMsgHandlerTest {

    //private Logger log = Logger.getLogger(this.getClass());

    private static final String DB_PATH = "/tmp/neo4j-test";
    private EjMsgHandler h = null;

    private org.ister.ej.Node node;
    private final OtpErlangAtom calltag = new OtpErlangAtom(MsgTag.CALL);

    private final OtpErlangPid pid = new OtpErlangPid("foo",0,0,0);
    private final MsgRef ref = new MsgRef(pid, new OtpErlangRef("dummy", 0, 0));

    private final OtpErlangTuple handler = getTwoTuple("handler",
            new OtpErlangAtom("graphdb") );
    private final OtpErlangTuple call = getTwoTuple("call",
            new OtpErlangAtom("scortch") );


    @Before
    public void setUp() throws Exception {
        // startup handler
        h = new EjMsgHandler(DB_PATH);
        node = new org.ister.ej.Node();
        node.setSelf(pid);
        h.init(node);
    }

    @After
    public void tearDown() throws Exception {
        //	db.shutdown();
    }

    @Test
    public void testScortch() throws Exception {
        OtpErlangTuple tuple = getPayload();
        Msg msg = new Msg(pid, ref, tuple);
        h.handle(msg);
//		assertEquals("index lookup",
//				     "{ok,[{result,true}]}", resp.getMsg().toString());
    }



    // TODO: put these in TestUtils somewhere shared
    private OtpErlangTuple getTwoTuple(String a, OtpErlangObject b) {
        OtpErlangObject[] list = new OtpErlangObject[]{
                new OtpErlangAtom(a), b };
        OtpErlangTuple handler = new OtpErlangTuple(list);
        return handler;
    }

    private OtpErlangTuple getPayload() {

        OtpErlangObject[] list = new OtpErlangObject[]{
                handler, call};
        OtpErlangList payload = new OtpErlangList(list);
        OtpErlangObject[] os  = new OtpErlangObject[]{calltag, payload};
        OtpErlangTuple tuple = new OtpErlangTuple(os);
        return tuple;
    }

}
