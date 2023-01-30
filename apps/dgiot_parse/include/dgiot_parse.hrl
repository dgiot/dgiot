%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-define(DEFAULT, <<"default">>).
-define(SLAVE, <<"slave">>).
-define(TIMESCALE, <<"timescale">>).
-define(TYPE, <<"PARSE">>).
-define(CACHE(Channel), binary_to_atom(<<?TYPE/binary, Channel/binary>>, utf8)).
-define(CLASS(ClassName), binary_to_atom(<<?TYPE/binary, "_class_", ClassName/binary>>, utf8)).
-define(DGIOT_PARSE_ETS, dgiot_parse_ets).
-define(ROLE_USER_ETS, role_user_ets).
-define(USER_ROLE_ETS, user_role_ets).
-define(ROLE_VIEWS_ETS, role_views_ets).
-define(ROLE_MENUVIEWS_ETS, role_menuviews_ets).
-define(USER_ETS, user_ets).
-define(ROLE_ETS, role_ets).
-define(ROLE_PARENT_ETS, role_parent_ets).
-define(PARENT_ROLE_ETS, parent_role_ets).
-define(NAME_ROLE_ETS, name_role_ets).
-define(ROLE_NAME_ETS, role_name_ets).
-define(CLASS_COUNT_ETS, parse_class_count_ets).


%%public enum ErrorCode
%%{
%%/// <summary>
%%/// Error code indicating that an unknown error or an error unrelated to Parse
%%/// occurred.
%%/// </summary>
%%OtherCause = -1,
%%
%%/// <summary>
%%/// Error code indicating that something has gone wrong with the server.
%%/// If you get this error code, it is Parse's fault. Please report the bug to
%%/// https://parse.com/help.
%%/// </summary>
%%InternalServerError = 1,
%%
%%/// <summary>
%%/// Error code indicating the connection to the Parse servers failed.
%%/// </summary>
%%ConnectionFailed = 100,
%%
%%/// <summary>
%%/// Error code indicating the specified object doesn't exist.
%%/// </summary>
%%ObjectNotFound = 101,
%%
%%/// <summary>
%%/// Error code indicating you tried to query with a datatype that doesn't
%%/// support it, like exact matching an array or object.
%%/// </summary>
%%InvalidQuery = 102,
%%
%%/// <summary>
%%/// Error code indicating a missing or invalid classname. Classnames are
%%/// case-sensitive. They must start with a letter, and a-zA-Z0-9_ are the
%%/// only valid characters.
%%/// </summary>
%%InvalidClassName = 103,
%%
%%/// <summary>
%%/// Error code indicating an unspecified object id.
%%/// </summary>
%%MissingObjectId = 104,
%%
%%/// <summary>
%%/// Error code indicating an invalid key name. Keys are case-sensitive. They
%%/// must start with a letter, and a-zA-Z0-9_ are the only valid characters.
%%/// </summary>
%%InvalidKeyName = 105,
%%
%%/// <summary>
%%/// Error code indicating a malformed pointer. You should not see this unless
%%/// you have been mucking about changing internal Parse code.
%%/// </summary>
%%InvalidPointer = 106,
%%
%%/// <summary>
%%/// Error code indicating that badly formed JSON was received upstream. This
%%/// either indicates you have done something unusual with modifying how
%%/// things encode to JSON, or the network is failing badly.
%%/// </summary>
%%InvalidJSON = 107,
%%
%%/// <summary>
%%/// Error code indicating that the feature you tried to access is only
%%/// available internally for testing purposes.
%%/// </summary>
%%CommandUnavailable = 108,
%%
%%/// <summary>
%%/// You must call Parse.initialize before using the Parse library.
%%/// </summary>
%%NotInitialized = 109,
%%
%%/// <summary>
%%/// Error code indicating that a field was set to an inconsistent type.
%%/// </summary>
%%IncorrectType = 111,
%%
%%/// <summary>
%%/// Error code indicating an invalid channel name. A channel name is either
%%/// an empty string (the broadcast channel) or contains only a-zA-Z0-9_
%%/// characters and starts with a letter.
%%/// </summary>
%%InvalidChannelName = 112,
%%
%%/// <summary>
%%/// Error code indicating that push is misconfigured.
%%/// </summary>
%%PushMisconfigured = 115,
%%
%%/// <summary>
%%/// Error code indicating that the object is too large.
%%/// </summary>
%%ObjectTooLarge = 116,
%%
%%/// <summary>
%%/// Error code indicating that the operation isn't allowed for clients.
%%/// </summary>
%%OperationForbidden = 119,
%%
%%/// <summary>
%%/// Error code indicating the result was not found in the cache.
%%/// </summary>
%%CacheMiss = 120,
%%
%%/// <summary>
%%/// Error code indicating that an invalid key was used in a nested
%%/// JSONObject.
%%/// </summary>
%%InvalidNestedKey = 121,
%%
%%/// <summary>
%%/// Error code indicating that an invalid filename was used for ParseFile.
%%/// A valid file name contains only a-zA-Z0-9_. characters and is between 1
%%/// and 128 characters.
%%/// </summary>
%%InvalidFileName = 122,
%%
%%/// <summary>
%%/// Error code indicating an invalid ACL was provided.
%%/// </summary>
%%InvalidACL = 123,
%%
%%/// <summary>
%%/// Error code indicating that the request timed out on the server. Typically
%%/// this indicates that the request is too expensive to run.
%%/// </summary>
%%Timeout = 124,
%%
%%/// <summary>
%%/// Error code indicating that the email address was invalid.
%%/// </summary>
%%InvalidEmailAddress = 125,
%%
%%/// <summary>
%%/// Error code indicating that a unique field was given a value that is
%%/// already taken.
%%/// </summary>
%%DuplicateValue = 137,
%%
%%/// <summary>
%%/// Error code indicating that a role's name is invalid.
%%/// </summary>
%%InvalidRoleName = 139,
%%
%%/// <summary>
%%/// Error code indicating that an application quota was exceeded. Upgrade to
%%/// resolve.
%%/// </summary>
%%ExceededQuota = 140,
%%
%%/// <summary>
%%/// Error code indicating that a Cloud Code script failed.
%%/// </summary>
%%ScriptFailed = 141,
%%
%%/// <summary>
%%/// Error code indicating that a Cloud Code validation failed.
%%/// </summary>
%%ValidationFailed = 142,
%%
%%/// <summary>
%%/// Error code indicating that deleting a file failed.
%%/// </summary>
%%FileDeleteFailed = 153,
%%
%%/// <summary>
%%/// Error code indicating that the application has exceeded its request limit.
%%/// </summary>
%%RequestLimitExceeded = 155,
%%
%%/// <summary>
%%/// Error code indicating that the provided event name is invalid.
%%/// </summary>
%%InvalidEventName = 160,
%%
%%/// <summary>
%%/// Error code indicating that the username is missing or empty.
%%/// </summary>
%%UsernameMissing = 200,
%%
%%/// <summary>
%%/// Error code indicating that the password is missing or empty.
%%/// </summary>
%%PasswordMissing = 201,
%%
%%/// <summary>
%%/// Error code indicating that the username has already been taken.
%%/// </summary>
%%UsernameTaken = 202,
%%
%%/// <summary>
%%/// Error code indicating that the email has already been taken.
%%/// </summary>
%%EmailTaken = 203,
%%
%%/// <summary>
%%/// Error code indicating that the email is missing, but must be specified.
%%/// </summary>
%%EmailMissing = 204,
%%
%%/// <summary>
%%/// Error code indicating that a user with the specified email was not found.
%%/// </summary>
%%EmailNotFound = 205,
%%
%%/// <summary>
%%/// Error code indicating that a user object without a valid session could
%%/// not be altered.
%%/// </summary>
%%SessionMissing = 206,
%%
%%/// <summary>
%%/// Error code indicating that a user can only be created through signup.
%%/// </summary>
%%MustCreateUserThroughSignup = 207,
%%
%%/// <summary>
%%/// Error code indicating that an an account being linked is already linked
%%/// to another user.
%%/// </summary>
%%AccountAlreadyLinked = 208,
%%
%%/// <summary>
%%/// Error code indicating that the current session token is invalid.
%%/// </summary>
%%InvalidSessionToken = 209,
%%
%%/// <summary>
%%/// Error code indicating that a user cannot be linked to an account because
%%/// that account's id could not be found.
%%/// </summary>
%%LinkedIdMissing = 250,
%%
%%/// <summary>
%%/// Error code indicating that a user with a linked (e.g. Facebook) account
%%/// has an invalid session.
%%/// </summary>
%%InvalidLinkedSession = 251,
%%
%%/// <summary>
%%/// Error code indicating that a service being linked (e.g. Facebook or
%%/// Twitter) is unsupported.
%%/// </summary>
%%UnsupportedService = 252
%%}
