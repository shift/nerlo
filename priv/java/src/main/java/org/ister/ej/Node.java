package org.ister.ej;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.log4j.Logger;

import com.ericsson.otp.erlang.OtpEpmd;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * This is always a hidden node in the cluster (due to jinterface).
 *
 * We aim only to have this node available to one particular peer node
 * so it is fine to have a hidden node. Usually, the peer will even start
 * this node.
 *
 * Once started you may control this node from an Erlang shell node.
 *
 * <pre>
 * $ erl -sname shell -setcookie 123456
 * (shell@host)1> {jnode, 'jnode@host'} ! {self(), {data, [{number,666}]}}.
 * (shell@host)1> {jnode, 'jnode@host'} ! {self(), {call, [{call,die}]}}.
 * </pre>
 *
 * If this has been started canonically from within Erlang with
 * ej_srv:start() you may send messages using ej_srv:send/2 or ej_srv:call/2,3.
 *
 * @author ingo
 */
public class Node implements Runnable {

	private static Node INSTANCE = null;

	private final String cookie;
	private final String nodename;
	private final String mboxname;
	private final String peernode;
	private static  Logger log;
	private final MsgHandler handler;

	private OtpNode node = null;
	private OtpMbox mbox = null;
	private OtpErlangPid self = null;

	private OtpErlangPid peerpid  = null;

	/**
	 * mostly for testing
	 */
	public Node() throws IOException {
		log = Logger.getLogger(this.getClass());
		this.peernode = "";
		this.nodename = "";
		this.mboxname = "";
		this.handler  = new SimpleMsgHandler();
		this.cookie   = "";
	}

	/**
	 * Create with custom setup.
	 *
	 * @param cookie
	 * @param name
	 * @param peer
	 */
	private Node(String name, String peer, Properties props) throws IOException {
		log = Logger.getLogger(this.getClass());
		this.cookie   = Main.getProperty(props, "ej.cookie", null);
		this.handler  = getHandler(props);
		this.nodename = name;
		this.mboxname = name;
		this.peernode = peer;
		this.node     = setupNode();
	}

	/**
	 * Get instance.
	 *
	 * @param cookie
	 * @param name
	 * @param peer
	 * @return
	 * @throws IOException
	 */
	public static Node getInstance(String name, String peer, Properties props)
			throws IOException {
		if (INSTANCE == null) {
			INSTANCE = new Node(name, peer, props);
		}
		return INSTANCE;
	}

	public static Node getInstance() throws IllegalStateException {
		if (INSTANCE == null) {
			throw new IllegalStateException("Node not initialized");
		}
		return INSTANCE;
	}

	public OtpNode getNode() {
		return node;
	}

	/**
	 * Run server loop.
	 *
	 * @throws Exception
	 */
	public void run() {

		this.handler.init(this);
		log.info("handler: " + this.handler.toString());

		try {
	        if (node.ping(peernode, 2000)) {
	            log.info(peernode + ": pong.");
	        } else {
	            log.warn(peernode + ": pang!");
	        }
		} catch (Exception e) {
			e.printStackTrace();
		}

        while (true) {
            try {
                OtpErlangObject o = this.mbox.receive();
                log.debug("message received: " + o.toString());
                if (o instanceof OtpErlangTuple) {
                	Msg msg = new Msg((OtpErlangTuple) o);
                	// TODO only allow messages from peer
                	processMsg(msg);
                } else {
                	throw new IllegalArgumentException("Tuple expected");
                }
	        } catch (OtpErlangDecodeException e) {
	            log.error("received message could not be decoded: " + e.toString());
	            continue;
	        } catch (OtpErlangExit e) {
	            log.error("remote pid " + e.pid() + " has terminated.");
	            continue;
	        } catch (IllegalArgumentException e) {
	        	log.error("parsing message: " + e.toString());
	        	continue;
            } catch (Exception e) {
            	String trace = "";
            	for (StackTraceElement st : e.getStackTrace()) {
            		trace += " -- " + st.toString();
            	}
            	log.fatal(e.toString() + trace);
            	System.exit(1);
            }
        }
    }

	public void stop() {
		this.handler.shutdown();
		this.mbox.exit("shutdown");
		OtpEpmd.unPublishPort(node);

//		MsgRef ref = new MsgRef(new OtpErlangPid("foo",0,0,0),
//					            new OtpErlangRef("dummy", 0, 0));
//		OtpErlangString ok    = new OtpErlangString(MsgTag.OK);
//		OtpErlangObject[] os  = new OtpErlangObject[2];
//		os[0] = ok;
//		os[1] = new OtpErlangList();
//		OtpErlangTuple payload = new OtpErlangTuple(os);
//		Msg msg = new Msg(getSelf(), ref, payload);
//		shutdown(msg, node);
	}

    public void sendPeer(Msg msg) {
    	if (this.peerpid == null) {
    		log.error("cannot send, have no pid of peer");
    		return;
    	}
    	OtpErlangTuple t = msg.toTuple();
    	log.debug("sending to " + this.peerpid + ": " + t.toString());
    	this.mbox.send(this.peerpid, t);
    }

	/**
	 *
	 * @return
	 */
	public OtpErlangPid getSelf() {
		return this.self;
	}

	// for testing only
	public void setSelf(OtpErlangPid self) {
		this.self = self;
	}

    /* PRIVATE */


    private void processMsg(Msg msg) throws Exception {
//    	OtpErlangPid pid = msg.getFrom();
		// TODO: why don't these log?
//    	log.debug("pid.node(): " + pid.node());
//    	log.debug("peernode  : " + this.peernode);
    	// TODO: re-enable this and compare the components,
    	//       or update this.peernode to be more fully qualified
//    	if (!pid.node().equals(this.peernode)) {
//    		log.error("message not allowed from: " + pid.toString());
//    		return;
//    	}
    	MsgTag tag = msg.getTag();
    	if (tag.equals(MsgTag.NODE)) {
    		if        (msg.match("call", "handshake")) {
	            handshake(msg);
    		} else if (msg.match("call", "shutdown")) {
	            shutdown(msg, node);
    		} else if (msg.match("call", "ping")) {
	            ping(msg);
    		} else {
    			AbstractMsgHandler.logUnhandledMsg(log, msg);
    		}
    	} else {
    		this.handler.handle(msg);
        }
    }

    private void ping(Msg msg) {
    	Map<String, Object> map = new HashMap<String, Object>(1);
        map.put("call", "ping");
        Msg answer = Msg.answer(this.self, MsgTag.OK, map, msg);
        sendPeer(answer);
    }

    private void handshake(Msg msg) {
    	this.peerpid = msg.getFrom();
    	log.info("handshake from: " + msg.getFrom().toString());
    	try {
	    	Map<String, Object> map = new HashMap<String, Object>(1);
	        map.put("call", "handshake");
	        Msg answer = Msg.answer(this.self, MsgTag.OK, map, msg);
	        sendPeer(answer);
    	} catch (Exception e) {
    		log.error("sending message failed in handshake: " + e.toString());
    	}
    }

    private void shutdown(Msg msg, OtpNode node) {
    	log.info("shutdown request from: " + msg.getFrom().toString());
    	this.handler.shutdown();
        try {
	    	Map<String, Object> map = new HashMap<String, Object>(1);
	        map.put("call", "bye");
	        Msg answer = Msg.answer(this.self, MsgTag.OK, map, msg);
	        sendPeer(answer);
        } catch (Exception e) {
    		log.error("sending message failed in shutdown: " + e.toString());
    	} finally {
	        OtpEpmd.unPublishPort(node);
	        log.info("bye ----");
	        System.exit(0);
    	}
    }

	private MsgHandler getHandler(Properties props) {
		String className = Main.getProperty(props, "ej.msgHandler", null);
		try {
			return (MsgHandler) Class.forName(className).newInstance();
		} catch (InstantiationException e) {
			log.error("cannot instantiate class: " + className);
			return new SimpleMsgHandler();
		} catch (IllegalAccessException e) {
			log.error("cannot access class: " + className);
			return new SimpleMsgHandler();
		} catch (ClassNotFoundException e) {
			log.error("class not found: " + className);
			return new SimpleMsgHandler();
		}
	}



    private OtpNode setupNode() throws IOException {
		log = Logger.getLogger(this.getClass());
        try {
            OtpNode node = new OtpNode(this.nodename, this.cookie);
            log.info("node running: " + this.nodename);
            log.info("peer: " + this.peernode);
//            if (OtpEpmd.publishPort(node)) {
//                log.info("node registered");
//            } else {
//                log.warn("node registration failed");
//                OtpEpmd.unPublishPort(node);
//                boolean res = OtpEpmd.publishPort(node);
//                log.debug("retry epmd connect results: " + res);
//            }
            String[] names = OtpEpmd.lookupNames();
            for (String name: names) {
                log.debug(name);
            }
            this.mbox = node.createMbox(this.mboxname);
            this.self = this.mbox.self();
            log.debug("self: " + this.self.toString());
            return node;
        } catch (IOException e) {
            log.fatal("no node\n" + e.toString());
            e.printStackTrace();
            throw e;
        }
    }

}