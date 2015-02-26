/*
 * SonarQube Erlang Plugin
 * Copyright (C) 2012 Tamas Kende
 * kende.tamas@gmail.com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02
 */
package org.sonar.erlang.parser;

import com.google.common.base.Joiner;
import org.junit.Test;
import org.sonar.sslr.parser.LexerlessGrammar;

import static org.sonar.sslr.tests.Assertions.assertThat;

public class ModuleAttributesTest {
  private LexerlessGrammar b = ErlangGrammarImpl.createGrammar();

  @Test
  public void moduleTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-module(m).");
  }

  @Test
  public void flowControlMacros() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code("-ifdef(debug).",
        "-define(LOG(X), io:format(\"{~p,~p}: ~p~n\", [?MODULE,?LINE,X])).",
        "-else.", "-define(LOG(X), true).", "-endif."));

  }

  @Test
  public void moduleAttrTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-ignore_xref([{json, decode, 1}]).");
  }

  @Test
  public void defineTest() {
    assertThat(b.rule(ErlangGrammarImpl.defineAttr))
      .matches(code("-define(TC_AWAIT_CANCEL_EVENT(),",
        "case megaco_tc_controller:lookup(block_on_cancel) of",
        "{value, {Tag, Pid}} when is_pid(Pid) ->", "Pid ! {Tag, self()},",
        "receive", "{Tag, Pid} ->", "ok", "end;",
        "{value, {sleep, To}} when is_integer(To) andalso (To > 0) ->",
        "receive after To -> ok end;", "_ ->", "ok", "end)."))
      .matches("-define(PARAM_TOKEN_TIMEOUT,                    60*15).")
      .matches("-define (is_uint16 (V), V >= 0, V =< 65535).")
      .matches("-define(TEST(B), ?LOG(??B ++ \" ~p~n\", [B])).")
      .matches("-define(IN_QUEUE(CState), CState =:= waiting; CState =:= originating; CState =:= ringing).")
      .matches("-define(SOME(A), B#c.A).");
  }

  @Test
  public void onLoadTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-on_load(init/0).");
  }

  @Test
  public void typeTest() {
    assertThat(b.rule(ErlangGrammarImpl.typeSpec))
      .matches("-type ascii_string() :: [1..255].")
      .matches(code("-type timestamp() :: {MegaSecs::non_neg_integer(), Secs::non_neg_integer(), MicroSecs::non_neg_integer()}."))
      .matches(code("-opaque my_opaq_type() :: Type."))
      .matches(code("-opaque codeserver() :: #codeserver{}."))
      .matches("-type http_version() :: 'HTTP/1.1' | 'HTTP/1.0'.");
  }

  @Test
  public void specTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-spec nif_now/0 :: ( ) -> timestamp().")
      .matches("-spec nif_rot13/1 :: ( ascii_string() ) -> ascii_string().")
      .matches(code("-spec init([", "non_neg_integer() | callback_module()]) ->",
        "{'ok', #state{  nodes::[],", "table::atom() | ets:tid(),",
        "host_names::maybe_improper_list()", "}", "}."))
      .matches("-spec in_neighbours(mfa_or_funlbl(), callgraph()) -> 'none' | [mfa_or_funlbl(),...].")
      .matches("-spec analyze(cerl:c_module()) -> {dict(), ordset('external' | label()), dict()}.")
      .matches("-spec method(c_module(), Param :: module:flyable(), Param2 :: module:stringiflyable()) -> module:ok_mokes(Id :: integer()).")
      .matches(code("-spec method(#b{}, {error, {db, any()}},",
        "(fun((id()) -> ok | {error, term()})))",
        "-> {{error, term()} | {ok, id()}, #b{}}."))
      .matches("-spec test_fun(any(), fun(() -> ok), pos_integer(), pos_integer()) -> {float()}.")
      .matches("-spec split_nodename(atom() | string()) -> {atom(), nonempty_string()}.")
      .matches("-spec test_fun(any(), fun(() -> ok), pos_integer(), pos_integer()) -> {float()}.")
      .matches("-spec doit(calendar:datetime(), calendar:datetime()) -> [reload | error | unmodified | gone].")
      .matches("-spec init([]) -> {ok, record(state)}.")
      .matches("-spec hexstring(binary()) -> string().")
      .matches("-spec join(list(I), Sep) -> list(I | Sep) when Sep :: term(), I :: term().")
      .matches("-spec now_ms({MegaSecs::pos_integer(),Secs::pos_integer(),MicroSecs::pos_integer()}) -> pos_integer().")
      .matches("-spec trace_named([atom()], pos_integer()) -> {ok, timer:tref()}.")
      .matches(code("-spec str(value(), Default::term(), Length::minmax()) -> {ok, ConvertedValue::term()} |",
        "{error, {overflow|underflow, term()}} | {default, term()}."))
      .matches("-spec i_parse_qs(String::string(), Acc::[{Key::string(), Value::string()}], Option::utf8) -> [{Key::string(), Value::string()}].")
      .matches("-spec check_access_pt(fun(), any(), {access_checks(), atom(), atom() | tuple(), atom() | string()}) -> any().")
      .matches("-spec(new/0 :: () -> a()).")
      .matches("-spec(n/1 :: (any()) -> boolean()).");
  }

  @Test
  public void exportTypeTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code("-export_type([compile_init_data/0,", "one_file_result/0,",
        "compile_result/0])."));
  }

  @Test
  public void importTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code("-import(erl_types,",
        "[any_none/1, t_any/0, t_atom/0, t_atom/1, t_atom_vals/1,",
        "t_binary/0, t_boolean/0,",
        "t_bitstr/0, t_bitstr/2, t_bitstr_concat/1, t_bitstr_match/2,",
        "t_cons/0, t_cons/2, t_cons_hd/1, t_cons_tl/1, t_contains_opaque/1,",
        "t_find_opaque_mismatch/2, t_float/0, t_from_range/2, t_from_term/1,",
        "t_fun/0, t_fun/2, t_fun_args/1, t_fun_range/1,",
        "t_inf/2, t_inf/3, t_inf_lists/2, t_inf_lists/3, t_inf_lists_masked/3,",
        "t_integer/0, t_integers/1,",
        "t_is_any/1, t_is_atom/1, t_is_atom/2, t_is_boolean/1, t_is_equal/2,",
        "t_is_integer/1, t_is_nil/1, t_is_none/1, t_is_none_or_unit/1,",
        "t_is_number/1, t_is_reference/1, t_is_pid/1, t_is_port/1,",
        "t_is_subtype/2, t_is_unit/1,",
        "t_limit/2, t_list/0, t_maybe_improper_list/0, t_module/0,",
        "t_none/0, t_non_neg_integer/0, t_number/0, t_number_vals/1,",
        "t_opaque_match_atom/2, t_opaque_match_record/2,",
        "t_opaque_matching_structure/2,",
        "t_pid/0, t_port/0, t_product/1, t_reference/0,",
        "t_sup/1, t_sup/2, t_subtract/2, t_to_string/2, t_to_tlist/1,",
        "t_tuple/0, t_tuple/1, t_tuple_args/1, t_tuple_subtypes/1,",
        "t_unit/0, t_unopaque/1])."))
      .matches("-import(?FILE, [test/1]).");
  }

  @Test
  public void compileTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code("-compile([{nowarn_deprecated_function,{gs,button,2}},",
        "{nowarn_deprecated_function,{gs,config,2}},",
        "{nowarn_deprecated_function,{gs,destroy,1}},",
        "{nowarn_deprecated_function,{gs,editor,2}},",
        "{nowarn_deprecated_function,{gs,entry,2}},",
        "{nowarn_deprecated_function,{gs,frame,2}},",
        "{nowarn_deprecated_function,{gs,label,2}},",
        "{nowarn_deprecated_function,{gs,listbox,2}},",
        "{nowarn_deprecated_function,{gs,menu,2}},",
        "{nowarn_deprecated_function,{gs,menubar,2}},",
        "{nowarn_deprecated_function,{gs,menubutton,2}},",
        "{nowarn_deprecated_function,{gs,menuitem,2}},",
        "{nowarn_deprecated_function,{gs,radiobutton,2}},",
        "{nowarn_deprecated_function,{gs,read,2}},",
        "{nowarn_deprecated_function,{gs,start,0}},",
        "{nowarn_deprecated_function,{gs,stop,0}},",
        "{nowarn_deprecated_function,{gs,window,2}}])."));
  }

  @Test
  public void fileTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-file(\"megaco_text_parser_prev3b.yrl\", 1593).");
  }

  @Test
  public void callbackTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code("-callback init(Args :: term()) ->", "{ok, State :: term()} |",
        "{ok, State :: term(), timeout() | hibernate} |",
        "{stop, Reason :: term()} | ignore."));
  }

  @Test
  public void customAttriutesTest() {
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches("-company({name, \"Dynamic Programmer\"}).")
      .matches("-author(\"Hernan Garcia\").")
      .matches("-awesome_module(true).");

  }

  @Test
  public void bugs(){
    assertThat(b.rule(ErlangGrammarImpl.module))
      .matches(code(" -define(GEN_FSM, p1_fsm).", "-behaviour(?GEN_FSM)."))
      .matches("-export_types([index_info/0]).")
      .matches(" -ifdef(NO_TRANSIENT_SUPERVISORS).\n" +
        " -define(SUPERVISOR_START, \n" +
        " gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,\n" +
        "    RoomShaper, Creator, Nick, DefRoomOpts],\n" +
        "        ?FSMOPTS)).\n" +
        "-else.\n" +
        "-define(SUPERVISOR_START, \n" +
        " Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),\n" +
        " supervisor:start_child(\n" +
        "   Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,\n" +
        "         Creator, Nick, DefRoomOpts])).\n" +
        "-endif.");
  }

  private static String code(String... lines) {
    return Joiner.on("\n").join(lines);
  }

}
