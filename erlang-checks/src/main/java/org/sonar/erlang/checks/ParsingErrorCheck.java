package org.sonar.erlang.checks;

import org.sonar.erlang.api.ErlangGrammar;

import com.sonar.sslr.api.AuditListener;
import com.sonar.sslr.api.RecognitionException;
import com.sonar.sslr.squid.checks.SquidCheck;
import org.sonar.check.Priority;
import org.sonar.check.Rule;

import java.io.PrintWriter;
import java.io.StringWriter;

@Rule(
    key = "ParsingError",
    priority = Priority.MAJOR)
public class ParsingErrorCheck extends SquidCheck<ErlangGrammar> implements AuditListener {

    public void processRecognitionException(RecognitionException e) {
        getContext().createLineViolation(this, e.getMessage(), e.getLine());
    }

    public void processException(Exception e) {
        StringWriter exception = new StringWriter();
        e.printStackTrace(new PrintWriter(exception));
        getContext().createFileViolation(this, exception.toString());
    }

}
