package org.ister.graphdb;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.apache.log4j.Logger;
import org.ister.ej.AbstractMsgHandler;
import org.ister.ej.ConcurrencyUtil;
import org.ister.ej.Msg;
import org.ister.ej.MsgTag;
import org.ister.ej.Node;
import org.ister.graphdb.executor.AbstractGraphdbMsgExecutor;
import org.ister.graphdb.executor.AddEdgeExecutor;
import org.ister.graphdb.executor.AddVertexExecutor;
import org.ister.graphdb.executor.DelEdgeExecutor;
import org.ister.graphdb.executor.DelPropertyExecutor;
import org.ister.graphdb.executor.DelVertexExecutor;
import org.ister.graphdb.executor.GetPropertiesExecutor;
import org.ister.graphdb.executor.GetPropertyExecutor;
import org.ister.graphdb.executor.IndexExecutor;
import org.ister.graphdb.executor.InfoExecutor;
import org.ister.graphdb.executor.SetPropertyExecutor;
import org.ister.graphdb.executor.VertexGetEdgesExecutor;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.kernel.EmbeddedGraphDatabase;

public class DbMsgHandler extends AbstractMsgHandler {

    private final Logger log;

    @SuppressWarnings("rawtypes")
    private final HashMap<String, Class> map = new HashMap<String, Class>();
    private final HashMap<String, AbstractGraphdbMsgExecutor> cache = new HashMap<String, AbstractGraphdbMsgExecutor>();

    private String path = "db";
    private GraphDatabaseService db = null;

    private final ExecutorService yielder = Executors.newSingleThreadExecutor();
    private final ExecutorService exec = Executors.newCachedThreadPool();
    private final CompletionService<Msg> service = new ExecutorCompletionService<Msg>(this.exec);

    public DbMsgHandler(String path) {
        log = Logger.getLogger(this.getClass());
        this.path = path;
    }

    public void init(Node node) {
        super.init(node);

        log.debug("db path: " + this.path);
        this.map.put("add_vertex", AddVertexExecutor.class);
        this.map.put("vertex_get_edges", VertexGetEdgesExecutor.class);
        this.map.put("del_vertex", DelVertexExecutor.class);
        this.map.put("add_edge", AddEdgeExecutor.class);
        this.map.put("del_edge", DelEdgeExecutor.class);
        this.map.put("set_property", SetPropertyExecutor.class);
        this.map.put("del_property", DelPropertyExecutor.class);
        this.map.put("get_property", GetPropertyExecutor.class);
        this.map.put("get_properties", GetPropertiesExecutor.class);
        this.map.put("info", InfoExecutor.class);
        this.map.put("index", IndexExecutor.class);

        setShutdownHook();

        final Node nodeRef = node;
        yielder.submit(new Runnable() {
            public void run(){
                log.debug("yielder entering run loop");
                while (true) {
                   try {
                          Future<Msg> fu = service.take();
                          nodeRef.sendPeer(fu.get());
                   } catch(ExecutionException e) {
                       log.error("Exception: \n" + e.toString());
                       ConcurrencyUtil.peelException(e.getCause());
                   } catch(InterruptedException e) {
                          log.error("Exception: \n" + e.toString());
                          Thread.currentThread().interrupt();
                   }
                }
            }
        });
        log.debug("yielder has been set");

        log.info("initialized: " + this.getClass().toString());
    }

    @Override
    public void handle(Msg msg) {
        Node node = getNode();
        Msg answer = handle_msg(msg, node);
        if( answer != null ) {
            node.sendPeer(answer);
        }
        return;
    }

    protected Msg handle_msg(Msg msg, Node node) {

        MsgTag tag = msg.getTag();

        if (tag.equals(MsgTag.CALL)) {
            if (msg.match("call", "init")) {
                return handle_init(msg, node);
            } else if (msg.match("call", "stop")) {
                dbShutdown();
                Map<String, Object> map = new HashMap<String, Object>(2);
                map.put("result", "ok");
                return  Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
            } else if (msg.match("call", "has_db")) {
                Map<String, Object> map = new HashMap<String, Object>(2);
                map.put("result", (this.db != null));
                return Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
            } else if (msg.has("call")) {
                if (this.db == null) { // || this.index == null
                    return  errorAnswer(msg, "no_db");
                }
                String id = (String) msg.get("call");
                AbstractGraphdbMsgExecutor ex = getExecutor(id);
                if (ex == null) {
                    return errorAnswer(msg, "no_executor");
                } else {
                    ex.setMsg(msg);
                    this.service.submit(ex);
                    return null;
                }
            }
        }

        logUnhandledMsg(log, msg);
        return errorAnswer(msg, "no_handle_msg_clause");

    }

    Msg handle_init(Msg msg, Node node) {
        Msg answer = null;
        if (dbInit(this.path)) {
            cache.clear();
            Map<String, Object> map = new HashMap<String, Object>(2);
            map.put("result", true);
            answer = Msg.answer(node.getSelf(), MsgTag.OK, map, msg);
        } else {
            answer = errorAnswer(msg, "no_db");
        }
        return answer;
    }

    @Override
    public void shutdown() {
        exec.shutdown();
        dbShutdown();
        yielder.shutdown();
    }


    private AbstractGraphdbMsgExecutor getExecutor(String id) {
        if (!cache.containsKey(id)) {
            if (!map.containsKey(id)) {
                log.error("no executor for '" + id);
                return null;
            }
            try {
                @SuppressWarnings({ "rawtypes" })
                Class clazz = map.get(id);
                AbstractGraphdbMsgExecutor ex = (AbstractGraphdbMsgExecutor) clazz.newInstance();
                ex.init(getNode().getSelf(), this.db);
                cache.put(id, ex);
            } catch (InstantiationException e) {
                log.error("failed to create executor for '" + id + "': " + e.toString());
                return null;
            } catch (IllegalAccessException e) {
                log.error("failed to create executor for '" + id + "': " + e.toString());
                return null;
            }
        }
        return cache.get(id);
    }

    private boolean dbInit(String path) {
        if (this.db == null) {
            return runDbInit(path);
        }
        return true;
    }

    private boolean runDbInit(String path) {
        try {
            this.db = new EmbeddedGraphDatabase(path);
            log.info("graph database initialized: " + path);
        } catch (Exception e) {
            log.error("initialization of database failed: " + e.toString());
            return false;
        }
        return true;
    }

    private void dbShutdown() {
        if (this.db instanceof GraphDatabaseService && this.db != null) {
            this.db.shutdown();
            this.db = null;
        }
        log.info("database shutdown completed");
    }

    private void setShutdownHook() {
        // almost always shutdown database
        final DbMsgHandler hdl = this;
        Thread shutdownThread = new Thread(new Runnable() {
            public void run(){
                hdl.dbShutdown();
            }
        });
        Runtime.getRuntime().addShutdownHook(shutdownThread);
        log.debug("shutdown hook has been set");
    }
}
