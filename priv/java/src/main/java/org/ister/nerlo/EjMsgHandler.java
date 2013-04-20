package org.ister.nerlo;

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.log4j.Logger;
import org.ister.ej.AbstractMsgHandler;
import org.ister.ej.Main;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;
import org.ister.graphdb.DbMsgHandler;
import org.ister.nerlo.example.SimpleFiber;

/**
 *
 * @author ingo
 *
 */
public class EjMsgHandler extends AbstractMsgHandler {

    private Logger log = Logger.getLogger(this.getClass());
    private final Bundle bundle = Bundle.getInstance();
    private final HashMap<String,AbstractMsgHandler> handlers = new HashMap<String,AbstractMsgHandler>();
    private String path = "db";
    private AbstractMsgHandler gdb;
    private Node node;

    public EjMsgHandler() {
        this(Main.getProperty("graphdb.db.path", "db"));
    }

    public EjMsgHandler(String path) {
        this.path = path;
    }

    public void init(Node node) {
        this.node = node;
        super.init(node);
        int_init();
    }

    @Override
    public void handle(Msg msg) {

        MsgTag tag = msg.getTag();
        if (tag.equals(MsgTag.CALL)) {
            if (msg.match("call", "job")) {
                job(msg);
                return;
            } else if (msg.match("call", "scorch")) {
                    scorch(msg);
                    return;
            } else if (msg.match("handler", "graphdb")) {
                handlers.get("graphdb").handle(msg);
                return;
            }
        }

        logUnhandledMsg(log, msg);
    }

    private void int_init() {
        gdb = new DbMsgHandler(path);
        gdb.init(node);
        handlers.put("graphdb", gdb);
        log.info("initialized: " + this.getClass().toString());
    }


    @SuppressWarnings("unchecked")
    private void job(Msg msg) {
        Node node = getNode();
        List<Long> l = bundle.parallelCopyRun(new SimpleFiber());
        for (Long res : l) {
            log.info("future returned: " + res);
        }
        Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("result", l.toString());
        Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
        node.sendPeer(answer);
    }

    @Override
    public void shutdown() {
        for (Entry<String, AbstractMsgHandler> e : handlers.entrySet()) {
            e.getValue().shutdown();
        }
        this.bundle.shutdown();
    }

    private void scorch(Msg msg) {
        log.info("scorch");

        handlers.remove("graphdb");
        gdb.shutdown();
        deleteRecursively(new File(path));
        new File(path).mkdir();
        int_init();

        Map<String, Object> map = new HashMap<String, Object>(2);
        map.put("result", "scorched");
        Msg answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
        node.sendPeer(answer);
    }

    // http://bit.ly/Q3Zk9s
    private boolean deleteRecursively(final File file) {
        if (file.exists()) {
            final File[] files = file.listFiles();
            for (int i = 0; i < files.length; i++) {
                if (files[i].isDirectory()) {
                    deleteRecursively(files[i]);
                }
                else {
                    files[i].delete();
                }
            }
        }
        return (file.delete());
    }

}
