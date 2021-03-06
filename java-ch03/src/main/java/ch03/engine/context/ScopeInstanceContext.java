package ch03.engine.context;

import ch03.data.TypedValue;
import ch03.engine.Execution;


/**
 * @author Tom Baeyens
 */
public class ScopeInstanceContext implements Context {

  Execution execution;

  public ScopeInstanceContext(Execution execution) {
    this.execution = execution;
  }

  @Override
  public TypedValue get(String key) {
    return null;
  }

  @Override
  public void set(String key, TypedValue value) {
  }
}
