package org.ister.graphdb.executor;

import org.apache.log4j.Logger;
import org.ister.nerlo.AbstractMsgExecutor;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.index.IndexManager;

import com.ericsson.otp.erlang.OtpErlangPid;

public abstract class AbstractGraphdbMsgExecutor extends AbstractMsgExecutor {

	protected final Logger log = Logger.getLogger(this.getClass());

	protected GraphDatabaseService db;
	protected IndexManager index_mgr;

	public void init(OtpErlangPid self, GraphDatabaseService db, IndexManager mgr) {
		super.init(self);
		this.db = db;
		this.index_mgr = mgr;
	}

}
