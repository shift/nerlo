package org.ister.graphdb.executor;

import org.apache.log4j.Logger;
import org.ister.ej.Main;
import org.ister.ej.Node;
import org.ister.nerlo.AbstractMsgExecutor;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.index.Index;

public abstract class AbstractGraphdbMsgExecutor extends AbstractMsgExecutor {

	protected final Logger log = Main.getLogger();
	
	protected GraphDatabaseService db;
	protected Index index;
	
	public void init(Node node, GraphDatabaseService db, Index index) {
		super.init(node);
		this.db = db;
		this.index = index;
	}

}
