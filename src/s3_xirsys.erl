-module(s3_xirsys).

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	create_user/2,
	create_user/3,
	create_user/4
	]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-type s3_bucket_acl() :: private
                       | public_read
                       | public_read_write

-spec create_user(string(), string()) -> ok.

create_user(Email, UserName) ->
    create_user(Email, UserName, private).

-spec create_user(string(), string(), s3_bucket_acl() | aws_config()) -> ok.

create_user(Email, UserName, Config)
  when is_record(Config, aws_config) ->
    create_user(Email, UserName, private, Config);

create_user(Email, UserName, ACL) ->
    create_user(Email, UserName, ACL, none).

-spec create_user(string(), string(), s3_bucket_acl(), aws_config()) -> ok.

create_user(Email, UserName, ACL, Config)
  when is_record(Config, aws_config) ->
    create_user(Email, UserName, ACL, none, Config).

-spec create_user(string(), string(), s3_bucket_acl(), aws_config()) -> ok.

create_user(Email, UserName, ACL, Config)
  when is_list(Email, UserName), is_atom(ACL), is_atom(LocationConstraint) ->
    Headers = case ACL of
                  private -> [];  %% private is the default
                  _       -> [{"x-amz-acl", erlcloud:encode_acl(ACL)}]
              end,
    POSTData = list_to_binary(io_lib:format("{'email': '~s', 'name': '~s'}", [Email, UserName])),
    erlcloud:s3_simple_request(Config, put, "user", "/", "", [], POSTData, Headers).